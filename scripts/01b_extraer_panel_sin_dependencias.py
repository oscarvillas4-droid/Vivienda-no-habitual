#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
01b_extraer_panel_sin_dependencias.py
===============================================================================
EXTRACTOR DEL EJERCICIO 2023 (para 2016, usar 01b_extraer_panel_2016.py)

Hace exactamente lo mismo y produce un fichero de salida idéntico, usando sólo
la biblioteca estándar de Python (que viene con cualquier instalación). Es algo
más lenta que la versión con numpy, pero sigue siendo muchísimo más rápida que
leer el ancho fijo desde R, porque recorta únicamente los bytes necesarios y
descarta pronto los registros que no interesan.

VERSIÓN 2 — exporta además estas casillas del fichero 8, que alimentan el
score de vivienda y la subsegmentación de 02_informe_arrendadores.R:
    SEGUROS_114        c.114  primas de seguro (suma de contratos)
    TRIBUTOS_115       c.115  tributos, recargos y tasas: IBI incluido (suma)
    AMORT_INM_131      c.131  amortización del inmueble (suma)
    VC_AMORT_123       c.123  valor catastral del bloque de amortización
    VC_CONSTR_124      c.124  valor catastral de la construcción
    IMPORTE_ADQ_126    c.126  importe de adquisición
    FECHA_CONTRATO_93  c.93   fecha del contrato de arrendamiento (la más antigua)
    FECHA_ADQ_120      c.120  fecha de adquisición (la más antigua)
La c.123 es clave: el arrendado a pleno año no rellena la c.83 (imputación),
pero sí la c.123 si deduce amortización, así que el valor catastral pasa a
estar disponible para la mayoría del segmento no habitual.
Si el panel ya está extraído con la versión anterior, no hace falta repetir la
extracción: 01c_enriquecer_panel.py añade estas mismas columnas (y la renta
del fichero 2) al CSV existente.

Uso (desde la consola de Windows, NO pegándolo en el intérprete interactivo):
    cd "C:\\...\\P HOGARES 2023\\2023"
    python 01b_extraer_panel_sin_dependencias.py

    python 01b_extraer_panel_sin_dependencias.py --datos "D:\\ruta" --salida "D:\\otra"

Salida:
    panel_arrendamientos_2023.csv[.gz]
    panel_arrendamientos_2023_meta.json

Después se ejecutan igualmente 02_informe_arrendadores.R y
03_estimacion_IVA_VUT_2023.R, que no necesitan ningún cambio.

Las posiciones, los criterios y las dos correcciones importantes (FACTORCAL con
10 decimales implícitos y casilla 149 en la posición 942) son las mismas que en
la versión con numpy; véanse allí los comentarios metodológicos completos.
===============================================================================
"""

from __future__ import annotations

import argparse
import csv
import gzip
import json
import sys
import math
import time
from collections import Counter, defaultdict
from pathlib import Path

# =============================================================================
# 1. CONFIGURACIÓN
# =============================================================================

RUTA_DATOS_DEFECTO = (
    r"C:\Users\ovillasf\OneDrive - Dirección General de Ordenación del Juego"
    r"\Documentos\P HOGARES 2023\2023"
)

ANIO_DEFECTO  = 2023
NOMBRE_SALIDA = "panel_arrendamientos_2023"   # lo recalcula aplicar_diseno()
LRECL_IDEN = 141
LRECL_RRII = 1061

# Diseño de registro POR EJERCICIO. El de 2023 está verificado contra
# 00_DiseñoRegistro_2023.xlsx (incluidas las casillas nuevas de la versión 2).
# Para otros años hay que confirmar posiciones: las casillas del IRPF cambian
# de numeración y de sitio de un año a otro. Mientras no se confirmen, el
# script explora el fichero, prueba el diseño de referencia y ABORTA si las
# comprobaciones de coherencia no lo respaldan.
DISENOS = {
    2023: {"verificado": True, "lrecl_iden": 141, "lrecl_rrii": 1061,
           "posiciones": None},               # None = las constantes de abajo
    2016: {"verificado": False, "lrecl_iden": None, "lrecl_rrii": None,
           "posiciones": None},               # pendiente de 00_DiseñoRegistro_2016
}
BYTES_POR_LECTURA = 64 * 1024 * 1024          # 64 MB por bloque de disco
CONSOLIDAR_POR_INMUEBLE = True

# Formato decimal por fichero. Los valores CON coma o punto siempre se leen en
# unidades. La duda son los valores de sólo dígitos: en un fichero de formato
# EXPLÍCITO son unidades enteras; en uno IMPLÍCITO llevan los decimales del
# diseño (céntimos, diezmilmillonésimas del factor). Se decide examinando los
# primeros registros; puede forzarse con "implicito" o "explicito".
MUESTRA_DETECCION    = 300_000
FORZAR_MODO_FACTOR   = None      # None = detección automática
FORZAR_MODO_IMPORTES = None      # None = detección automática

# Comprimir la salida. False por defecto: data.table::fread() necesita el
# paquete R.utils para abrir un .gz, y no siempre se puede instalar.
COMPRIMIR_SALIDA = False

# (posición inicial 1-based, longitud) -> se convierten en cortes de bytes
def corte(ini: int, largo: int) -> slice:
    return slice(ini - 1, ini - 1 + largo)

# Fichero 1
C_IDENPER_I   = corte(1, 11)
C_CPRO        = corte(25, 2)
C_CMUN        = corte(27, 3)
C_FACTORCAL   = corte(36, 20)

# Fichero 8: criba (se lee siempre)
C_URBANA_67    = 47 - 1
C_RUSTICA_68   = 48 - 1
C_AAEE_72      = 51 - 1
C_SITUAC_65    = 46 - 1           # situación: 1=refcat común, 2=PV/Nav, 3=sin refcat, 4=extranjero
C_DISPOS_73    = 52 - 1
C_ACCESORIO_74 = 53 - 1
C_ARRENDAM_75  = 54 - 1
C_DIAS_VH_76   = corte(55, 6)      # días como vivienda habitual del propietario
C_DIAS_EXCON_79 = corte(61, 6)    # días vivienda de hijos/excónyuge
C_DIAS_AAEE_80  = corte(67, 6)    # días afecto a actividades económicas
C_DIAS_NEG_82   = corte(73, 6)    # días arrendamiento de negocio
C_DIAS_DISP_85  = corte(100, 6)   # días a disposición del titular
C_MARCA_100    = 147 - 1
C_INGRESOS_102 = corte(154, 20)

# Fichero 8: resto (sólo para los registros seleccionados)
C_IDENPER   = corte(1, 11)
C_IDENHOG   = corte(12, 11)
C_REFCAT    = corte(23, 11)
C_PROP_63   = corte(34, 6)
C_USUF_64   = corte(40, 6)
C_VALCAT_83 = corte(79, 20)
C_FCON_93   = corte(139, 8)       # [v2] fecha del contrato de arrendamiento
C_DIAS_101  = corte(148, 6)
C_REP_106   = corte(234, 20)
C_REPAP_107 = corte(254, 20)
C_COM_109   = corte(294, 20)
C_FORM_110  = corte(314, 20)
C_JUR_111   = corte(334, 20)
C_SERV_112  = corte(354, 20)
C_SUM_113   = corte(374, 20)
C_SEG_114   = corte(394, 20)      # [v2] primas de seguro
C_TRIB_115  = corte(414, 20)      # [v2] tributos y tasas (IBI)
C_FADQ_120  = corte(476, 8)       # [v2] fecha de adquisición
C_VC_123    = corte(498, 20)      # [v2] valor catastral (bloque amortización)
C_VCC_124   = corte(518, 20)      # [v2] valor catastral de la construcción
C_ADQ_126   = corte(558, 20)      # [v2] importe de adquisición
C_AMORTI_131 = corte(658, 20)     # [v2] amortización del inmueble
C_RNETO_149 = corte(942, 20)
C_REDUC_150 = corte(962, 20)

BLANCO = 32      # ord(' ')
CERO = 48        # ord('0')

PROVINCIAS = {
    1: "Araba/Álava", 2: "Albacete", 3: "Alicante/Alacant", 4: "Almería",
    5: "Ávila", 6: "Badajoz", 7: "Balears, Illes", 8: "Barcelona",
    9: "Burgos", 10: "Cáceres", 11: "Cádiz", 12: "Castellón/Castelló",
    13: "Ciudad Real", 14: "Córdoba", 15: "Coruña, A", 16: "Cuenca",
    17: "Girona", 18: "Granada", 19: "Guadalajara", 20: "Gipuzkoa",
    21: "Huelva", 22: "Huesca", 23: "Jaén", 24: "León", 25: "Lleida",
    26: "Rioja, La", 27: "Lugo", 28: "Madrid", 29: "Málaga", 30: "Murcia",
    31: "Navarra", 32: "Ourense", 33: "Asturias", 34: "Palencia",
    35: "Palmas, Las", 36: "Pontevedra", 37: "Salamanca",
    38: "Santa Cruz de Tenerife", 39: "Cantabria", 40: "Segovia",
    41: "Sevilla", 42: "Soria", 43: "Tarragona", 44: "Teruel", 45: "Toledo",
    46: "Valencia/València", 47: "Valladolid", 48: "Bizkaia", 49: "Zamora",
    50: "Zaragoza", 51: "Ceuta", 52: "Melilla",
}

CAPITALES = {
    1: 59, 2: 3, 3: 14, 4: 13, 5: 19, 6: 15, 7: 40, 8: 19, 9: 59, 10: 37,
    11: 12, 12: 40, 13: 34, 14: 21, 15: 30, 16: 78, 17: 79, 18: 87, 19: 130,
    20: 69, 21: 41, 22: 125, 23: 50, 24: 89, 25: 120, 26: 89, 27: 28, 28: 79,
    29: 67, 30: 30, 31: 201, 32: 54, 33: 44, 34: 120, 35: 16, 36: 38, 37: 274,
    38: 38, 39: 75, 40: 194, 41: 91, 42: 173, 43: 148, 44: 216, 45: 168,
    46: 250, 47: 186, 48: 20, 49: 275, 50: 297, 51: 1, 52: 1,
}

COLUMNAS = ["IDENPER", "IDENHOG", "REFCAT", "CPRO", "CMUN", "provincia",
            "es_capital", "FACTORCAL", "PCT_PROP_63", "PCT_USUF_64", "share",
            "ARRENDAM_75", "MARCA_RED_100", "uso", "DIAS_101", "DIAS_VH_76",
            "DIAS_DISP_85", "DIAS_AAEE_80", "DIAS_NEG_82", "DIAS_EXCON_79",
            "A_DISPOSICION_73", "uso_mixto", "es_negocio_aaee", "SITUACION_65", "n_contratos",
            "INGRESOS_102", "VALOR_CAT_83", "REND_NETO_149", "REDUCCION_150",
            "G_REPARA_106", "G_REP_APL_107", "G_COMUN_109", "G_FORMAL_110",
            "G_JURID_111", "G_SERVPER_112", "G_SUMIN_113",
            "n_viv_persona", "n_viv_hogar",
            # [v2] casillas adicionales para el score de vivienda
            "SEGUROS_114", "TRIBUTOS_115", "AMORT_INM_131",
            "VC_AMORT_123", "VC_CONSTR_124", "IMPORTE_ADQ_126",
            "FECHA_CONTRATO_93", "FECHA_ADQ_120"]

# =============================================================================
# 2. UTILIDADES DE LECTURA
# =============================================================================


def _a_float(t: bytes):
    """Número con separador decimal explícito. La coma se trata como separador
    decimal; si además hay punto, el punto separa miles ("1.234,56")."""
    if b"," in t:
        t = t.replace(b".", b"").replace(b",", b".")
    try:
        return float(t)
    except ValueError:
        return None


MODO_FACTOR_EXPLICITO   = False    # los fija la detección automática
MODO_IMPORTES_EXPLICITO = False
DIV_FACTOR  = 1e10                 # decimales implícitos de FACTORCAL
DIV_IMPORTE = 100.0                # decimales implícitos de los importes


def num_flex(b: bytes, divisor: float):
    """Campo numérico de ancho fijo con decimales implícitos del diseño.

    Si el valor trae separador decimal EXPLÍCITO (coma o punto) ya viene en
    unidades y no se divide; si son sólo dígitos, se aplican los decimales
    implícitos (`divisor`). Devuelve None si el campo está en blanco.
    """
    t = b.strip().replace(b" ", b"")
    if not t:
        return None
    if b"," in t or b"." in t:
        return _a_float(t)
    # Sólo dígitos: en modo explícito ya son unidades; si no, decimales implícitos
    if (divisor >= 1e9 and MODO_FACTOR_EXPLICITO) or \
       (divisor == 100.0 and MODO_IMPORTES_EXPLICITO):
        return _a_float(t)
    try:
        return int(t) / divisor
    except ValueError:
        return _a_float(t)


def ent(b: bytes):
    v = num_flex(b, 1)
    return None if v is None else int(v)


def ent0(b: bytes) -> int:
    v = ent(b)
    return 0 if v is None else v


def dec2(b: bytes):
    return num_flex(b, DIV_IMPORTE)


def dec2_0(b: bytes) -> float:
    v = num_flex(b, DIV_IMPORTE)
    return 0.0 if v is None else v


def detectar_separadores(ruta: Path, lrecl: int, cortes: dict, max_reg: int):
    """Mira en los primeros registros si los campos traen coma o punto decimal
    y recoge ejemplos crudos para mostrarlos por pantalla."""
    visto = {c: False for c in cortes}
    ejemplos = {c: [] for c in cortes}
    gen = registros(ruta, lrecl)
    for i, r in enumerate(gen):
        if i >= max_reg:
            break
        for nombre, cr in cortes.items():
            b = r[cr]
            if (b"," in b) or (b"." in b):
                visto[nombre] = True
            if len(ejemplos[nombre]) < 3 and b.strip():
                ejemplos[nombre].append(b.decode("ascii", "replace").strip())
    gen.close()
    return visto, ejemplos


def maximo(a, b):
    """Máximo tolerante a valores ausentes."""
    if a is None:
        return b
    if b is None:
        return a
    return a if a > b else b


def minimo(a, b):
    """Mínimo tolerante a valores ausentes (para las fechas más antiguas)."""
    if a is None:
        return b
    if b is None:
        return a
    return a if a < b else b


def detectar_stride(ruta: Path, lrecl: int) -> tuple[int, str]:
    with open(ruta, "rb") as f:
        cabeza = f.read(lrecl * 2 + 8)
    pos = cabeza.find(b"\n")
    if pos == -1:
        return lrecl, "sin salto de línea"
    if pos == lrecl:
        return lrecl + 1, "LF"
    if pos == lrecl + 1 and cabeza[lrecl:lrecl + 1] == b"\r":
        return lrecl + 2, "CRLF"
    raise ValueError(
        f"{ruta.name}: el primer salto de línea aparece en el byte {pos}, "
        f"incompatible con una longitud de registro de {lrecl}."
    )


def inferir_lrecl(ruta: Path):
    """Longitud de registro deducida del primer salto de línea; None si el
    fichero no lleva saltos (habrá que tomarla del diseño)."""
    with open(ruta, "rb") as f:
        cabeza = f.read(8192)
    pos = cabeza.find(b"\n")
    if pos == -1:
        return None
    return pos - 1 if pos and cabeza[pos - 1:pos] == b"\r" else pos


def explorar(ruta: Path, etiqueta: str, n_reg: int = 5) -> None:
    """Diagnóstico de un fichero de ancho fijo cuyo diseño no está confirmado:
    longitud de registro, tamaño, y mapa de qué tipo de carácter aparece en
    cada posición (d=dígitos, A=letras, .=blancos, ,=separador decimal).
    Sirve para localizar los campos sin tener el Excel del diseño delante."""
    lrecl = inferir_lrecl(ruta)
    tam = ruta.stat().st_size
    print(f"\n--- EXPLORACIÓN: {etiqueta} ({ruta.name}) ---")
    print(f"    Tamaño: {tam:,} bytes")
    if lrecl is None:
        print("    Sin saltos de línea. Longitudes de registro compatibles con "
              "el tamaño del fichero:")
        divisores = [d for d in range(80, 3000) if tam % d == 0]
        print("   ", divisores[:20] if divisores else "(ninguna entre 80 y 3000)")
        return
    stride, salto = detectar_stride(ruta, lrecl)
    print(f"    Longitud de registro: {lrecl} (salto {salto}); "
          f"{tam // stride:,} registros")
    clases = ["."] * lrecl
    with open(ruta, "rb") as f:
        muestra = [f.read(stride)[:lrecl] for _ in range(20_000)]
    for r in muestra:
        if len(r) < lrecl:
            break
        for i, byte in enumerate(r):
            c = chr(byte)
            if c.isdigit():
                clases[i] = "d" if clases[i] in (".", "d") else "?"
            elif c in ",.":
                clases[i] = ","
            elif c == " ":
                pass
            else:
                clases[i] = "A" if clases[i] in (".", "A") else "?"
    print("    Mapa de posiciones (bloques de 10; regla cada 100):")
    for ini in range(0, lrecl, 100):
        trozo = "".join(clases[ini:ini + 100])
        marcado = " ".join(trozo[j:j + 10] for j in range(0, len(trozo), 10))
        print(f"    {ini + 1:>5}: {marcado}")
    print("    Primeros registros en crudo:")
    for r in muestra[:n_reg]:
        print("      |" + r.decode("latin-1").rstrip() + "|")


def validar_diseno(f_iden: Path, f_rrii: Path, n_reg: int = 50_000) -> list[str]:
    """Comprueba que las posiciones aplicadas caen donde deben. No basta con
    que los valores sean numéricos: un campo desplazado suele aterrizar sobre
    ceros de relleno y pasaría desapercibido. Por eso se mira también la forma
    del contenido (las marcas son caracteres, no dígitos), la variabilidad y la
    coherencia entre campos relacionados."""
    fallos = []
    NUMERICO = set(b"0123456789 +-.,")

    # ---- fichero 1 -----------------------------------------------------------
    n = ident_ok = cpro_ok = 0
    factores, factor_no_num = [], 0
    for r in registros(f_iden, LRECL_IDEN):
        if n >= n_reg:
            break
        n += 1
        if (ent(r[C_IDENPER_I]) or 0) > 0:
            ident_ok += 1
        if 1 <= ent0(r[C_CPRO]) <= 52:
            cpro_ok += 1
        if not set(r[C_FACTORCAL]) <= NUMERICO:
            factor_no_num += 1
        f = num_flex(r[C_FACTORCAL], DIV_FACTOR)
        if f:
            factores.append(f)
    if n == 0:
        return ["el fichero de identificación no tiene registros legibles"]
    if ident_ok / n < 0.99:
        fallos.append(f"identificador de declarante ilegible en el "
                      f"{100 * (1 - ident_ok / n):.1f}% del fichero 1")
    if cpro_ok / n < 0.95:
        fallos.append(f"código de provincia fuera de 01-52 en el "
                      f"{100 * (1 - cpro_ok / n):.1f}% de los registros")
    if factor_no_num / n > 0.01:
        fallos.append(f"el campo del factor de elevación contiene caracteres no "
                      f"numéricos en el {100 * factor_no_num / n:.1f}% de los casos")
    if len(factores) / n < 0.95:
        fallos.append(f"factor de elevación nulo o ilegible en el "
                      f"{100 * (1 - len(factores) / n):.1f}% de los registros")
    elif len(set(factores)) < 5 and n > 100:
        fallos.append("el factor de elevación apenas varía: la posición no parece "
                      "la correcta")
    elif factores:
        medio = sum(factores) / len(factores)
        if not (1.0 <= medio <= 1000.0):
            fallos.append(f"factor de elevación medio implausible ({medio:,.2f}); "
                          f"revisa la posición o los decimales")

    # ---- fichero 8 -----------------------------------------------------------
    marcas = {"c.67 urbana": C_URBANA_67,
              "c.68 rústica": C_RUSTICA_68, "c.72 act. económica": C_AAEE_72,
              "c.74 accesorio": C_ACCESORIO_74, "c.75 arrendamiento": C_ARRENDAM_75,
              "c.100 reducción": C_MARCA_100}
    numericos = {"c.63 % propiedad": (C_PROP_63, DIV_IMPORTE, 0, 100),
                 "c.83 valor catastral": (C_VALCAT_83, DIV_IMPORTE, 0, 1e8),
                 "c.102 ingresos": (C_INGRESOS_102, DIV_IMPORTE, 0, 1e7),
                 "c.123 valor catastral (amort.)": (C_VC_123, DIV_IMPORTE, 0, 1e8),
                 "c.131 amortización inmueble": (C_AMORTI_131, DIV_IMPORTE, 0, 1e7),
                 "c.150 reducción": (C_REDUC_150, DIV_IMPORTE, 0, 1e7)}
    n = arrendados = con_ingresos = con_dias = dias_ok = 0
    malas_marcas = {k: 0 for k in marcas}
    no_numerico = {k: 0 for k in numericos}
    fuera_rango = {k: 0 for k in numericos}
    for r in registros(f_rrii, LRECL_RRII):
        if n >= n_reg:
            break
        n += 1
        for nombre, idx in marcas.items():
            b = r[idx:idx + 1]
            if b.isdigit():            # una marca nunca es un dígito
                malas_marcas[nombre] += 1
        for nombre, (cr, div, lo, hi) in numericos.items():
            trozo = r[cr]
            if not set(trozo) <= NUMERICO:
                no_numerico[nombre] += 1
            v = num_flex(trozo, div)
            if v is not None and not (lo <= v <= hi):
                fuera_rango[nombre] += 1
        if r[C_ARRENDAM_75:C_ARRENDAM_75 + 1].strip():
            arrendados += 1
            if (num_flex(r[C_INGRESOS_102], DIV_IMPORTE) or 0) > 0:
                con_ingresos += 1
            d = ent0(r[C_DIAS_101])
            if d > 0:
                con_dias += 1
                if d <= 366:
                    dias_ok += 1
    if n == 0:
        return ["el fichero de capital inmobiliario no tiene registros legibles"]
    for nombre, malas in malas_marcas.items():
        if malas / n > 0.01:
            fallos.append(f"la marca {nombre} cae sobre dígitos en el "
                          f"{100 * malas / n:.1f}% de los registros: está desplazada")
    for nombre, malas in no_numerico.items():
        if malas / n > 0.01:
            fallos.append(f"el campo {nombre} contiene caracteres no numéricos "
                          f"en el {100 * malas / n:.1f}% de los registros")
    for nombre, malas in fuera_rango.items():
        if malas / n > 0.05:
            fallos.append(f"el campo {nombre} queda fuera de rango en el "
                          f"{100 * malas / n:.1f}% de los registros")
    if arrendados == 0:
        fallos.append("ningún registro trae la marca de arrendamiento (c.75): "
                      "esa posición no es la correcta")
    else:
        if con_ingresos / arrendados < 0.5:
            fallos.append(f"sólo el {100 * con_ingresos / arrendados:.1f}% de los "
                          f"inmuebles arrendados declara ingresos: la casilla de "
                          f"ingresos íntegros no cuadra")
        if con_dias / arrendados < 0.5:
            fallos.append(f"sólo el {100 * con_dias / arrendados:.1f}% de los "
                          f"inmuebles arrendados tiene días de arrendamiento: la "
                          f"casilla de días no cuadra")
        elif dias_ok / max(con_dias, 1) < 0.95:
            fallos.append("los días de arrendamiento se salen de 1-366 con "
                          "demasiada frecuencia")
    return fallos


# Campos de una sola posición (marcas): se guardan como índice, no como corte
MARCAS = {"URBANA_67", "RUSTICA_68", "AAEE_72", "ACCESORIO_74", "ARRENDAM_75",
          "MARCA_100"}
# nombre en el JSON -> nombre de la constante del script
NOMBRES_CORTE = {
    "IDENPER_I": "C_IDENPER_I", "CPRO": "C_CPRO", "CMUN": "C_CMUN",
    "FACTORCAL": "C_FACTORCAL", "IDENPER": "C_IDENPER", "IDENHOG": "C_IDENHOG",
    "REFCAT": "C_REFCAT", "PROP_63": "C_PROP_63", "USUF_64": "C_USUF_64",
    "VALCAT_83": "C_VALCAT_83", "DIAS_101": "C_DIAS_101",
    "INGRESOS_102": "C_INGRESOS_102", "REP_106": "C_REP_106",
    "REPAP_107": "C_REPAP_107", "COM_109": "C_COM_109", "FORM_110": "C_FORM_110",
    "JUR_111": "C_JUR_111", "SERV_112": "C_SERV_112", "SUM_113": "C_SUM_113",
    "RNETO_149": "C_RNETO_149", "REDUC_150": "C_REDUC_150",
    "URBANA_67": "C_URBANA_67", "RUSTICA_68": "C_RUSTICA_68",
    "AAEE_72": "C_AAEE_72", "ACCESORIO_74": "C_ACCESORIO_74",
    "ARRENDAM_75": "C_ARRENDAM_75", "MARCA_100": "C_MARCA_100",
    # [v2] casillas adicionales
    "FCON_93": "C_FCON_93", "SEG_114": "C_SEG_114", "TRIB_115": "C_TRIB_115",
    "FADQ_120": "C_FADQ_120", "VC_123": "C_VC_123", "VCC_124": "C_VCC_124",
    "ADQ_126": "C_ADQ_126", "AMORTI_131": "C_AMORTI_131",
}


def cargar_diseno_json(ruta_datos: Path, anio: int,
                       explicito: str | None = None) -> dict | None:
    """Lee diseno_<anio>.json, el que escribe 00_diseno_a_config.py a partir
    del Excel oficial de diseño de registro del panel."""
    f = Path(explicito) if explicito else ruta_datos / f"diseno_{anio}.json"
    if not f.is_file():
        return None
    cfg = json.loads(f.read_text(encoding="utf-8"))
    print(f"      Diseño cargado de {f.name}"
          + ("" if cfg.get("verificado") else "  (contiene hipótesis por confirmar)"))
    return cfg


def aplicar_posiciones(cfg: dict) -> list[str]:
    """Fija las constantes de posición con el diseño del JSON. Devuelve la
    lista de campos que no venían en la configuración."""
    global LRECL_IDEN, LRECL_RRII, DIV_FACTOR, DIV_IMPORTE
    if cfg.get("lrecl_iden"):
        LRECL_IDEN = int(cfg["lrecl_iden"])
    if cfg.get("lrecl_rrii"):
        LRECL_RRII = int(cfg["lrecl_rrii"])
    pos = cfg.get("posiciones") or {}
    faltan = []
    for nombre, constante in NOMBRES_CORTE.items():
        c = pos.get(nombre)
        if not c:
            faltan.append(nombre)
            continue
        ini, largo = int(c["inicio"]), int(c["longitud"] or 1)
        globals()[constante] = (ini - 1) if nombre in MARCAS else corte(ini, largo)
    if pos.get("FACTORCAL") and pos["FACTORCAL"].get("decimales") is not None:
        DIV_FACTOR = 10.0 ** int(pos["FACTORCAL"]["decimales"])
    if pos.get("INGRESOS_102") and pos["INGRESOS_102"].get("decimales") is not None:
        DIV_IMPORTE = 10.0 ** int(pos["INGRESOS_102"]["decimales"])
    print(f"      Posiciones aplicadas: {len(NOMBRES_CORTE) - len(faltan)}"
          f"/{len(NOMBRES_CORTE)} campos | longitudes {LRECL_IDEN} y {LRECL_RRII}"
          f" | decimales factor {int(math.log10(DIV_FACTOR))}, "
          f"importes {int(math.log10(DIV_IMPORTE))}")
    if faltan:
        print("      SIN POSICIÓN (se usarán las del diseño de referencia):",
              ", ".join(faltan))
    return faltan


def aplicar_diseno(anio: int, como: int | None = None) -> dict:
    """Fija longitudes de registro y nombre de salida para el ejercicio."""
    global LRECL_IDEN, LRECL_RRII, NOMBRE_SALIDA
    d = DISENOS.get(anio, {"verificado": False, "lrecl_iden": None,
                           "lrecl_rrii": None, "posiciones": None})
    ref = DISENOS[como] if como else d
    LRECL_IDEN = d["lrecl_iden"] or ref["lrecl_iden"] or LRECL_IDEN
    LRECL_RRII = d["lrecl_rrii"] or ref["lrecl_rrii"] or LRECL_RRII
    NOMBRE_SALIDA = f"panel_arrendamientos_{anio}"
    return d


def registros(ruta: Path, lrecl: int):
    """Genera los registros del fichero como objetos bytes de longitud lrecl."""
    stride, _ = detectar_stride(ruta, lrecl)
    sobra = b""
    with open(ruta, "rb") as f:
        while True:
            bloque = f.read(BYTES_POR_LECTURA)
            if not bloque:
                break
            datos = sobra + bloque
            n = len(datos) // stride
            for i in range(n):
                ini = i * stride
                yield datos[ini:ini + lrecl]
            sobra = datos[n * stride:]
    if len(sobra) >= lrecl:                 # último registro sin salto final
        yield sobra[:lrecl]


def localizar(ruta_datos: Path, patrones: list[str]) -> Path:
    # Prioridad a los ficheros del propio panel, que empiezan por "_"
    # ("_1_IDEN2023.TXT"), frente a catálogos como "11_IDEN2023.TXT".
    for pat in patrones:
        encontrados = sorted(ruta_datos.glob(pat),
                             key=lambda q: (not q.name.startswith("_"), q.name))
        if encontrados:
            return encontrados[0]
    presentes = [p.name for p in sorted(ruta_datos.iterdir())][:25]
    raise FileNotFoundError(
        f"No se encuentra ningún fichero que case con {patrones[0]} en "
        f"{ruta_datos}.\nFicheros presentes: {presentes}"
    )


def num_csv(v) -> str:
    """Formato numérico estable para el CSV (vacío si falta el dato)."""
    if v is None:
        return ""
    if isinstance(v, float):
        if v != v:                          # NaN
            return ""
        if v == int(v) and abs(v) < 1e15:
            return f"{int(v)}.0"
        return repr(round(v, 10))
    return str(v)


# =============================================================================
# 3. PROCESO PRINCIPAL
# =============================================================================


def extraer(ruta_datos: Path, ruta_salida: Path, anio: int = ANIO_DEFECTO,
            diseno_como: int | None = None, forzar: bool = False,
            solo_explorar: bool = False, diseno_json: str | None = None) -> None:
    global MODO_FACTOR_EXPLICITO, MODO_IMPORTES_EXPLICITO
    t0 = time.time()
    meta: dict = {"generado": time.strftime("%Y-%m-%d %H:%M:%S"),
                  "ruta_datos": str(ruta_datos),
                  "motor": "biblioteca estándar (sin numpy ni pandas)",
                  "version_extractor": 2}

    # ------------------------------------------------------------- fichero 1
    d = aplicar_diseno(anio, diseno_como)
    cfg = cargar_diseno_json(ruta_datos, anio, diseno_json)
    if cfg:
        aplicar_posiciones(cfg)
        if cfg.get("verificado"):
            d = dict(d, verificado=True)
    f_iden = localizar(ruta_datos, [f"*1_IDEN{anio}*", "*1_IDEN*.TXT",
                                    "*1_IDEN*.txt", f"*IDEN*{anio}*"])
    f_rrii_pre = localizar(ruta_datos, [f"*8_IRPF{anio}*RRII*", "*8_IRPF*RRII*.txt",
                                        "*8_IRPF*RRII*.TXT", "*RRII*"])

    # Con un diseño no confirmado, primero se mira el fichero y se comprueba
    # que las posiciones de referencia producen valores con sentido.
    if solo_explorar or not d["verificado"]:
        explorar(f_iden, "fichero 1 (identificación)")
        explorar(f_rrii_pre, "fichero 8 (capital inmobiliario)")
        li, lr = inferir_lrecl(f_iden), inferir_lrecl(f_rrii_pre)
        if li:
            LRECL_IDEN_DET, LRECL_RRII_DET = li, lr or LRECL_RRII
            globals()["LRECL_IDEN"] = LRECL_IDEN_DET
            globals()["LRECL_RRII"] = LRECL_RRII_DET
            print(f"\n    Longitudes detectadas: fichero 1 = {LRECL_IDEN_DET}, "
                  f"fichero 8 = {LRECL_RRII_DET}")
            ref = DISENOS.get(diseno_como or ANIO_DEFECTO, {})
            if (LRECL_IDEN_DET, LRECL_RRII_DET) == (ref.get("lrecl_iden"),
                                                    ref.get("lrecl_rrii")):
                print("    Coinciden con el diseño de "
                      f"{diseno_como or ANIO_DEFECTO}: se prueban sus posiciones.")
            else:
                print(f"    NO coinciden con el diseño de "
                      f"{diseno_como or ANIO_DEFECTO}: las posiciones de las "
                      "casillas serán distintas.")
        if solo_explorar:
            return
        print("\n[0/4] Validando el diseño de registro sobre datos reales...")
        fallos = validar_diseno(f_iden, f_rrii_pre)
        if fallos:
            print("      COMPROBACIONES FALLIDAS:")
            for f in fallos:
                print("       -", f)
            if not forzar:
                raise SystemExit(
                    f"\nEl diseño aplicado no encaja con los ficheros de {anio}.\n"
                    "No se genera la base: los resultados serían basura.\n"
                    "Solución recomendada: generar el diseño desde el Excel "
                    "oficial del panel,\n"
                    "    python 00_diseno_a_config.py --diseno "
                    f"\"...\\00_DiseñoRegistro_{anio}.xlsx\" --anio {anio} "
                    f"--salida \"{ruta_datos}\"\n"
                    "y repetir esta orden: el extractor cargará diseno_"
                    f"{anio}.json automáticamente.\n"
                    "Alternativas: --explorar para diagnosticar, o --forzar "
                    "para continuar de todos modos (resultados no fiables).")
            print("      Se continúa por --forzar (resultados NO fiables).")
        else:
            print("      Todas las comprobaciones de coherencia pasan.")
    stride, salto = detectar_stride(f_iden, LRECL_IDEN)
    print(f"[1/4] Identificación: {f_iden.name} "
          f"({f_iden.stat().st_size // stride:,} registros, salto {salto})")

    visto1, ej1 = detectar_separadores(f_iden, LRECL_IDEN,
                                       {"FACTORCAL": C_FACTORCAL},
                                       MUESTRA_DETECCION)
    MODO_FACTOR_EXPLICITO = (visto1["FACTORCAL"] if FORZAR_MODO_FACTOR is None
                             else FORZAR_MODO_FACTOR == "explicito")
    print("      Formato de FACTORCAL:",
          "separador decimal explícito" if MODO_FACTOR_EXPLICITO
          else "entero con 10 decimales implícitos",
          "| ejemplos:", " | ".join(repr(e) for e in ej1["FACTORCAL"]) or "(vacío)")
    meta["anio"] = anio
    meta["diseno_verificado"] = bool(d["verificado"])
    meta["modo_factorcal"] = "explicito" if MODO_FACTOR_EXPLICITO else "implicito"

    geo: dict[int, tuple[int, int, float]] = {}
    poblacion = 0.0
    for i, r in enumerate(registros(f_iden, LRECL_IDEN), 1):
        idp = ent(r[C_IDENPER_I])
        if idp is None:
            continue
        f10 = num_flex(r[C_FACTORCAL], DIV_FACTOR)  # decimales del diseño
        factor = 0.0 if f10 is None else f10        # (o separador explícito)
        geo[idp] = (ent0(r[C_CPRO]), ent0(r[C_CMUN]), factor)
        poblacion += factor
        if i % 500_000 == 0:
            print(f"      {i:,} registros leídos", end="\r")
    print(f"      {len(geo):,} declarantes leídos                    ")
    print(f"      Población estimada de declarantes: {poblacion:,.0f}")
    meta["declarantes_muestra"] = len(geo)
    meta["declarantes_poblacion_estimada"] = round(poblacion, 1)
    if not (5e6 < poblacion < 6e7):
        print("      AVISO IMPORTANTE: esa cifra no es verosímil (se esperan "
              "decenas de millones). Revisa los decimales de FACTORCAL: aquí "
              "se asumen 10.")
        meta["aviso_factorcal"] = "población estimada fuera del rango esperado"

    # ------------------------------------------------------------- fichero 8
    f_rrii = f_rrii_pre
    stride8, salto8 = detectar_stride(f_rrii, LRECL_RRII)
    n_est = f_rrii.stat().st_size // stride8
    print(f"[2/4] Capital inmobiliario: {f_rrii.name}")
    print(f"      ~{n_est:,} registros, salto {salto8}, "
          f"{f_rrii.stat().st_size / 1e9:.2f} GB")

    cortes_imp = {"INGRESOS_102": C_INGRESOS_102, "VALOR_CAT_83": C_VALCAT_83,
                  "REDUCCION_150": C_REDUC_150, "REND_NETO_149": C_RNETO_149,
                  "PCT_PROP_63": C_PROP_63}
    visto8, ej8 = detectar_separadores(f_rrii, LRECL_RRII, cortes_imp,
                                       MUESTRA_DETECCION)
    MODO_IMPORTES_EXPLICITO = (any(visto8.values()) if FORZAR_MODO_IMPORTES is None
                               else FORZAR_MODO_IMPORTES == "explicito")
    print("      Formato de importes y porcentajes:",
          "separador decimal explícito" if MODO_IMPORTES_EXPLICITO
          else "enteros con 2 decimales implícitos (céntimos)",
          "| ejemplos c.102:",
          " | ".join(repr(e) for e in ej8["INGRESOS_102"]) or "(vacío)")
    meta["modo_importes"] = "explicito" if MODO_IMPORTES_EXPLICITO else "implicito"

    embudo = {"registros_totales": 0, "con_ingresos": 0,
              "con_ingresos_y_marca_75": 0, "tras_exclusiones": 0}

    # clave -> lista mutable con los acumuladores del inmueble
    #  0 IDENPER 1 IDENHOG 2 REFCAT 3 prop 4 usuf 5 valcat 6 arr75 7 marca100
    #  8 dias 9 ingresos 10 rep106 11 repap107 12 com109 13 form110 14 jur111
    # 15 serv112 16 sum113 17 rneto149 18 reduc150
    # 19 diasVH76 20 diasDISP85 21 diasAAEE80 22 diasNEG82 23 diasEXC79
    # 24 dispos73 25 n_contratos 26 situ65
    # [v2] 27 seg114 28 trib115 29 amorti131 30 vc123 31 vcc124 32 adq126
    #      33 fcon93 34 fadq120
    inmuebles: dict = {}
    sin_ref = 0

    for r in registros(f_rrii, LRECL_RRII):
        embudo["registros_totales"] += 1

        ingresos = dec2(r[C_INGRESOS_102])
        if ingresos is None or ingresos <= 0:
            continue
        embudo["con_ingresos"] += 1

        arr75 = r[C_ARRENDAM_75] not in (BLANCO, CERO, 0)
        if arr75:
            embudo["con_ingresos_y_marca_75"] += 1

        # Exclusiones estructurales: afecto a actividad económica, arrendamiento
        # accesorio y finca rústica pura. La marca 75 se conserva como columna
        # para que R decida, igual que en la versión con numpy.
        if r[C_AAEE_72] not in (BLANCO, CERO, 0):
            continue
        if r[C_ACCESORIO_74] not in (BLANCO, CERO, 0):
            continue
        rustica = r[C_RUSTICA_68] not in (BLANCO, CERO, 0)
        urbana = r[C_URBANA_67] not in (BLANCO, CERO, 0)
        if rustica and not urbana:
            continue
        embudo["tras_exclusiones"] += 1

        idp = ent0(r[C_IDENPER])
        refcat = ent(r[C_REFCAT])
        if refcat is None or refcat <= 0:
            sin_ref += 1
            clave = ("SR", sin_ref)                 # sin referencia: unidad propia
            refcat = -1
        else:
            clave = (idp, refcat)

        fila = inmuebles.get(clave)
        valores = (
            ent0(r[C_DIAS_101]),
            ingresos,
            dec2_0(r[C_REP_106]), dec2_0(r[C_REPAP_107]), dec2_0(r[C_COM_109]),
            dec2_0(r[C_FORM_110]), dec2_0(r[C_JUR_111]), dec2_0(r[C_SERV_112]),
            dec2_0(r[C_SUM_113]), dec2_0(r[C_RNETO_149]), dec2_0(r[C_REDUC_150]),
            ent0(r[C_DIAS_VH_76]),                       # 11: días vivienda habitual
            ent0(r[C_DIAS_DISP_85]),                     # 12: días a disposición
            ent0(r[C_DIAS_AAEE_80]),                     # 13: días afecto a AAEE
            ent0(r[C_DIAS_NEG_82]),                      # 14: días arrend. de negocio
            ent0(r[C_DIAS_EXCON_79]),                    # 15: días excónyuge/hijos
        )
        marca100 = r[C_MARCA_100] not in (BLANCO, CERO, 0)
        dispos73 = r[C_DISPOS_73] not in (BLANCO, CERO, 0)
        sit_b = r[C_SITUAC_65]
        situ65 = sit_b - CERO if 49 <= sit_b <= 57 else 0   # '1'..'9' -> 1..9
        # Estos tres se consolidan por máximo, no por suma: se conserva el
        # valor ausente como tal, igual que en la versión con numpy.
        prop = dec2(r[C_PROP_63])
        usuf = dec2(r[C_USUF_64])
        valcat = dec2(r[C_VALCAT_83])
        # [v2] casillas adicionales: los gastos y la amortización se SUMAN al
        # consolidar contratos; los atributos del inmueble (valores catastrales
        # del bloque de amortización, importe de adquisición) se toman por
        # máximo, y las fechas por mínimo (la más antigua). El 0 se trata como
        # ausente en los atributos y en las fechas.
        seg114 = dec2_0(r[C_SEG_114])
        trib115 = dec2_0(r[C_TRIB_115])
        amorti131 = dec2_0(r[C_AMORTI_131])
        vc123 = dec2(r[C_VC_123]) or None
        vcc124 = dec2(r[C_VCC_124]) or None
        adq126 = dec2(r[C_ADQ_126]) or None
        fcon93 = ent(r[C_FCON_93]) or None
        fadq120 = ent(r[C_FADQ_120]) or None

        if fila is None or not CONSOLIDAR_POR_INMUEBLE:
            if fila is not None:                    # sin consolidar: clave única
                clave = ("SR", -embudo["tras_exclusiones"])
            #  7 marca100  8..23 valores(16)  24 dispos73  25 n_contratos  26 situ65
            inmuebles[clave] = [idp, ent0(r[C_IDENHOG]), refcat, prop, usuf,
                                valcat, arr75, marca100, *valores, dispos73, 1,
                                situ65,
                                seg114, trib115, amorti131,
                                vc123, vcc124, adq126, fcon93, fadq120]
        else:
            fila[3] = maximo(fila[3], prop)
            fila[4] = maximo(fila[4], usuf)
            fila[5] = maximo(fila[5], valcat)
            fila[6] = fila[6] or arr75
            fila[7] = fila[7] or marca100
            for k in range(16):                     # sumar días(por finalidad), ingresos, gastos
                fila[8 + k] += valores[k]
            fila[24] = fila[24] or dispos73
            fila[25] += 1
            # [v2]
            fila[27] += seg114
            fila[28] += trib115
            fila[29] += amorti131
            fila[30] = maximo(fila[30], vc123)
            fila[31] = maximo(fila[31], vcc124)
            fila[32] = maximo(fila[32], adq126)
            fila[33] = minimo(fila[33], fcon93)
            fila[34] = minimo(fila[34], fadq120)

        if embudo["registros_totales"] % 500_000 == 0:
            print(f"      {embudo['registros_totales']:,} / ~{n_est:,} registros"
                  f"  (seleccionados {embudo['tras_exclusiones']:,})", end="\r")

    print(f"      {embudo['registros_totales']:,} registros procesados, "
          f"{embudo['tras_exclusiones']:,} seleccionados            ")

    tasa75 = embudo["con_ingresos_y_marca_75"] / max(embudo["con_ingresos"], 1)
    meta["tasa_marca_75"] = round(tasa75, 4)
    print(f"      Marca de arrendamiento (c.75) en el {100 * tasa75:.1f}% "
          f"de los registros con ingresos")

    # ------------------------------------------------- derivación y recuentos
    print("[3/4] Consolidando contratos y derivando variables")
    meta["registros_antes_consolidar"] = embudo["tras_exclusiones"]
    meta["inmuebles_tras_consolidar"] = len(inmuebles)
    meta["inmuebles_con_varios_contratos"] = sum(
        1 for f in inmuebles.values() if f[25] > 1)

    # Sólo cuentan los inmuebles cuyo titular aparece en el fichero de
    # identificación, para que los recuentos cuadren con las filas escritas.
    validos = [f for f in inmuebles.values() if f[0] in geo]
    sin_factor = len(inmuebles) - len(validos)
    if sin_factor:
        print(f"      AVISO: {sin_factor:,} inmuebles sin correspondencia en el "
              f"fichero de identificación; se descartan.")
    meta["inmuebles_con_factor"] = len(validos)

    n_viv_persona = Counter(f[0] for f in validos)
    n_viv_hogar = Counter(f[1] for f in validos)

    # --------------------------------------------------- diagnóstico casilla 149
    holguras = []
    for f in validos[:200_000]:
        gastos = f[11] + f[12] + f[13] + f[14] + f[15] + f[16]   # 107,109..113
        holguras.append(f[9] - gastos - f[17])
    if holguras:
        holguras.sort()
        meta["c149_diagnostico"] = {
            "descripcion": ("Diferencia entre 102 - (107,109..113) y la casilla "
                            "149. Debe ser >= 0 y recoger el resto de gastos "
                            "deducibles, sobre todo amortizaciones."),
            "mediana_eur": round(holguras[len(holguras) // 2], 2),
            "pct_negativa": round(
                100 * sum(1 for h in holguras if h < -1) / len(holguras), 2),
        }
    # [v2] cobertura de las casillas nuevas en el segmento arrendado
    if validos:
        con_vc123 = sum(1 for f in validos if f[30])
        con_amorti = sum(1 for f in validos if f[29] > 0)
        con_trib = sum(1 for f in validos if f[28] > 0)
        meta["cobertura_v2"] = {
            "vc_amort_123_pct": round(100 * con_vc123 / len(validos), 1),
            "amort_inm_131_pct": round(100 * con_amorti / len(validos), 1),
            "tributos_115_pct": round(100 * con_trib / len(validos), 1),
        }
        print(f"      [v2] Cobertura: VC c.123 {100 * con_vc123 / len(validos):.1f}% | "
              f"amortización c.131 {100 * con_amorti / len(validos):.1f}% | "
              f"tributos c.115 {100 * con_trib / len(validos):.1f}%")

    # --------------------------------------------------------------- escritura
    print("[4/4] Escribiendo la base analítica")
    ruta_salida.mkdir(parents=True, exist_ok=True)
    if COMPRIMIR_SALIDA:
        destino = ruta_salida / f"{NOMBRE_SALIDA}.csv.gz"
        abrir = lambda: gzip.open(destino, "wt", newline="", encoding="utf-8",
                                  compresslevel=1)
    else:
        destino = ruta_salida / f"{NOMBRE_SALIDA}.csv"
        abrir = lambda: open(destino, "w", newline="", encoding="utf-8")

    escritas = 0
    f_min, f_max, f_sum = float("inf"), 0.0, 0.0
    ing_w, veq_w = 0.0, 0.0
    with abrir() as fh:
        w = csv.writer(fh, delimiter=";", lineterminator="\n")
        w.writerow(COLUMNAS)
        for f in validos:
            cpro, cmun, factor = geo[f[0]]
            share = max(f[3] or 0.0, f[4] or 0.0) / 100.0
            if share <= 0 or share > 1:
                share = 1.0
            uso = "hab" if (f[7] or f[18] > 0) else "tur"
            dias_arr  = float(f[8])                       # c.101 días arrendado
            dias_vh   = float(f[19]); dias_disp = float(f[20])
            dias_aaee = float(f[21]); dias_neg  = float(f[22])
            dias_exc  = float(f[23]); dispos    = bool(f[24])
            # USO MIXTO (criterio estricto): el inmueble estuvo arrendado pero
            # TAMBIEN tuvo cualquier dia de otro uso (vivienda habitual del dueno
            # c.76, a disposicion c.85/c.73, o vivienda de excónyuge c.79). Su
            # casilla 101 no refleja explotacion en alquiler todo el año. El
            # universo depurado los excluye; la AEAT los separa por finalidad.
            uso_mixto = 1 if (dias_arr > 0 and
                              (dias_vh > 0 or dias_disp > 0 or dispos or dias_exc > 0)) else 0
            # NEGOCIO / AAEE por días: aunque la marca c.72 (afecto a AAEE) ya
            # excluye estructuralmente, algunos inmuebles arrendados declaran
            # dias de arrendamiento de NEGOCIO (c.82) o dias afectos a AAEE
            # (c.80) sin marca 72. Se marcan para excluirlos del no habitual.
            es_negocio_aaee = 1 if (dias_neg > 0 or dias_aaee > 0) else 0
            f_min = min(f_min, factor); f_max = max(f_max, factor)
            f_sum += factor
            ing_w += f[9] * factor
            veq_w += share * factor
            w.writerow([
                f[0], f[1], f[2], f"{cpro:02d}", f"{cmun:03d}",
                PROVINCIAS.get(cpro, ""),
                1 if CAPITALES.get(cpro) == cmun else 0,
                num_csv(factor), num_csv(f[3]), num_csv(f[4]), num_csv(share),
                1 if f[6] else 0, 1 if f[7] else 0, uso,
                num_csv(dias_arr), num_csv(dias_vh),
                num_csv(dias_disp), num_csv(dias_aaee), num_csv(dias_neg),
                num_csv(dias_exc),
                1 if dispos else 0, uso_mixto, es_negocio_aaee,
                f[26] if len(f) > 26 else 0, f[25],
                num_csv(f[9]), num_csv(f[5]), num_csv(f[17]), num_csv(f[18]),
                num_csv(f[10]), num_csv(f[11]), num_csv(f[12]), num_csv(f[13]),
                num_csv(f[14]), num_csv(f[15]), num_csv(f[16]),
                n_viv_persona[f[0]], n_viv_hogar[f[1]],
                # [v2]
                num_csv(f[27]), num_csv(f[28]), num_csv(f[29]),
                num_csv(f[30]), num_csv(f[31]), num_csv(f[32]),
                num_csv(f[33]), num_csv(f[34]),
            ])
            escritas += 1

    meta["fichero_csv"] = destino.name
    meta["filas_salida"] = escritas
    meta["embudo"] = embudo
    meta["segundos"] = round(time.time() - t0, 1)
    (ruta_salida / f"{NOMBRE_SALIDA}_meta.json").write_text(
        json.dumps(meta, indent=2, ensure_ascii=False), encoding="utf-8")

    print(f"      {destino.name}  ({destino.stat().st_size / 1e6:.1f} MB, "
          f"{escritas:,} filas)")
    print("\n" + "=" * 74)
    print("  EXTRACCIÓN COMPLETADA")
    print("=" * 74)
    print(f"  Registros del fichero 8 procesados : {embudo['registros_totales']:,}")
    print(f"  Con ingresos de arrendamiento      : {embudo['con_ingresos']:,}")
    print(f"  Tras exclusiones estructurales     : {embudo['tras_exclusiones']:,}")
    print(f"  Inmuebles tras consolidar contratos: {meta['inmuebles_tras_consolidar']:,}"
          f"  ({meta['inmuebles_con_varios_contratos']:,} con más de un contrato)")
    print(f"  Filas escritas                     : {escritas:,}")
    if escritas:
        print(f"  Factor de elevación (mín/medio/máx): "
              f"{f_min:.4f} / {f_sum / escritas:.4f} / {f_max:.4f}")
        print(f"  Viviendas en alquiler (elevadas)   : {veq_w:,.0f}")
        if veq_w > 0:
            print(f"  Ingreso íntegro medio por vivienda : {ing_w / veq_w:,.0f} EUR")
        meta["factor_medio"] = round(f_sum / escritas, 6)
        meta["viviendas_elevadas"] = round(veq_w, 1)
        meta["ingreso_medio_por_vivienda"] = (round(ing_w / veq_w, 2)
                                              if veq_w else None)
    print(f"  Tiempo total                       : {meta['segundos']} s")
    print("=" * 74)
    print("  Siguiente paso: python 01c_enriquecer_panel.py --datos ... para")
    print("                  añadir la RENTA del declarante (fichero 2_Renta);")
    print("                  después, 02_informe_arrendadores.R")
    print("                  (para 2016, usar antes 01b_extraer_panel_2016.py)")
    print("=" * 74)


def main() -> None:
    p = argparse.ArgumentParser(description="Extracción del panel IRPF 2023 "
                                            "(sin dependencias externas)")
    p.add_argument("--datos", default=RUTA_DATOS_DEFECTO)
    p.add_argument("--salida", default=None)
    p.add_argument("--anio", type=int, default=ANIO_DEFECTO,
                   help="ejercicio a extraer (2023, 2016...)")
    p.add_argument("--diseno-json", default=None,
                   help="ruta del diseno_<anio>.json (por defecto, en --datos)")
    p.add_argument("--diseno-como", type=int, default=None,
                   help="usar las posiciones de otro ejercicio ya verificado")
    p.add_argument("--explorar", action="store_true",
                   help="sólo diagnosticar el formato de los ficheros")
    p.add_argument("--forzar", action="store_true",
                   help="extraer aunque fallen las comprobaciones de coherencia")
    args = p.parse_args()

    ruta_datos = Path(args.datos)
    if not ruta_datos.is_dir():
        sys.exit(f"No existe la carpeta de datos: {ruta_datos}")
    extraer(ruta_datos, Path(args.salida) if args.salida else ruta_datos,
            anio=args.anio, diseno_como=args.diseno_como, forzar=args.forzar,
            solo_explorar=args.explorar, diseno_json=args.diseno_json)


if __name__ == "__main__":
    main()
