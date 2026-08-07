# -*- coding: utf-8 -*-
"""
01d_cruzar_inmuebles.py — cruza el panel de arrendamientos 2023 con el
MODULO INMOBILIARIO del Panel de Hogares (Doc. IEF 4/2025) por la referencia
catastral anonimizada (RC_ANONIMA), que es la misma REFCAT seudonimizada del
fichero 8. Anade al panel las variables que faltaban para replicar a la AEAT:

  INQ_DOMICILIADO   1 si ALGUIEN DISTINTO de los titulares tiene su VIVIENDA
                    HABITUAL en ese inmueble (fichero VIVHAB) -> arrendada como
                    vivienda habitual del inquilino (el criterio real del 'Si'
                    de la AEAT, hasta ahora inobservable).
  N_VIVHAB_RC       personas con vivienda habitual en la RC (cualquiera).
  N_VIVHAB_AJENOS   idem excluyendo a los titulares (INM_PR + panel).
  PROV_INM, MUN_INM ubicacion REAL del inmueble (INM_CARACT) -> permite el
                    contraste por ubicacion como la AEAT, no por residencia.
  VIV_RC            numero de viviendas en la RC segun Catastro (>=1 = uso
                    residencial: filtro directo de garaje/local/trastero).
  VIV_METROS_RC     superficie de las viviendas de la RC (m2, replica el m2 AEAT).
  ANCONS_RC         anio de construccion.
  VALCAT_RC         valor catastral de la RC (INM_CARACT).
  URBACLAVE_RC      clave de uso Catastro (V=residencial...) si consta en VIVHAB.
  DIAS_RC, NDECL_RC (opcional, --con-fichero8) dias de arrendamiento agregados
                    por RC entre TODOS los declarantes del fichero 8 (suma
                    topada a 365 y numero de declarantes): los "dias censales"
                    que la AEAT ve y la muestra individual no.

Uso:
  python 01d_cruzar_inmuebles.py --datos "C:\\...\\P HOGARES 2023\\2023" ^
         --modulo "C:\\...\\MODULO INMOBILIARIO"     [--con-fichero8]

Si --modulo no se indica, busca los .TXT del modulo en la carpeta de --datos.
Deja copia de seguridad .bak del panel y escribe enriquecer_inmuebles_log.txt.
"""

import argparse
import csv
import os
import sys
import time
from pathlib import Path

RUTA_DATOS_DEFECTO = (
    r"C:\Users\ovillasf\OneDrive - Dirección General de Ordenación del Juego"
    r"\Documentos\P HOGARES 2023\2023"
)

# Carpeta del MODULO INMOBILIARIO 2023 (VIVHAB / INM_PR / INM_CARACT). Ojo:
# cuelga de "...\P HOGARES 2023\..." SIN la subcarpeta "Documentos", a
# diferencia del panel de arrendamientos. Se deja fijada para no teclearla; si
# se pasa --modulo, ese valor manda.
RUTA_MODULO_DEFECTO = (
    r"C:\Users\ovillasf\OneDrive - Dirección General de Ordenación del Juego"
    r"\P HOGARES 2023\MODULO INMOBILIARIO 2023\MODULO INMOBILIARIO 2023"
    r"\MODULO INMOBILIARIO 2023\panel_inmob_txt"
)

# ---- diseños de registro (DiseñoRegistro_Inmuebles_2022_2023.xlsx) ----------
# posiciones 1-based inclusive
DISENOS = {
    2023: dict(
        VIVHAB=dict(lrecl=29, IDENPER=(1, 11), TIPO=(12, 13),
                    URBACLAVE=(14, 14), RC=(15, 25), MARCA=(26, 29)),
        INM_PR=dict(lrecl=61, IDENPER=(1, 11), CODERE=(12, 13),
                    PORBIN=(14, 19), VALORC=(20, 34), FECHIN=(35, 42),
                    FECHFI=(43, 50), RC=(51, 61)),
        INM_CA=dict(lrecl=82, CA=(1, 2), PROV=(3, 4), MUN=(5, 7),
                    DIST=(8, 9), SECC=(10, 12), VIVLOC=(13, 17),
                    VIVLOC_M=(18, 32), VIV=(33, 37), VIV_M=(38, 52),
                    ANCONS=(53, 56), RC=(57, 67), VALCAT=(68, 82)),
        pats=dict(vh=["vivhab2023", "vivhab_2023"],
                  pr=["inm_pr2023", "inm_pr_2023"],
                  ca=["inm_caract2023", "inm_caract_2023"]),
        panel="panel_arrendamientos_2023.csv"),
    2016: dict(   # diseno 2016-2021: mismas posiciones, lrecl 26/62/83
        VIVHAB=dict(lrecl=26, IDENPER=(1, 11), TIPO=(12, 13),
                    URBACLAVE=(14, 14), RC=(15, 25)),
        INM_PR=dict(lrecl=62, IDENPER=(1, 11), CODERE=(12, 13),
                    PORBIN=(14, 19), VALORC=(20, 34), FECHIN=(35, 42),
                    FECHFI=(43, 50), RC=(51, 61)),
        INM_CA=dict(lrecl=83, CA=(1, 2), PROV=(3, 4), MUN=(5, 7),
                    DIST=(8, 9), SECC=(10, 12), VIVLOC=(13, 17),
                    VIVLOC_M=(18, 32), VIV=(33, 37), VIV_M=(38, 52),
                    ANCONS=(53, 56), RC=(57, 67), VALCAT=(68, 82)),
        pats=dict(vh=["vivhab2016", "vivhab_2016"],
                  pr=["inm_pr2016", "inm_pr_2016"],
                  ca=["inm_caract2016", "inm_caract_2016"]),
        panel="panel_arrendamientos_2016.csv"),
}
INM_PR = dict(lrecl=61, IDENPER=(1, 11), CODERE=(12, 13), PORBIN=(14, 19),
              VALORC=(20, 34), FECHIN=(35, 42), FECHFI=(43, 50), RC=(51, 61))
INM_CA = dict(lrecl=82, CA=(1, 2), PROV=(3, 4), MUN=(5, 7), DIST=(8, 9),
              SECC=(10, 12), VIVLOC=(13, 17), VIVLOC_M=(18, 32), VIV=(33, 37),
              VIV_M=(38, 52), ANCONS=(53, 56), RC=(57, 67), VALCAT=(68, 82))
# fichero 8 (solo si --con-fichero8): REFCAT y dias/ingresos
F8 = dict(lrecl=1061, IDENPER=(1, 11), REFCAT=(23, 33), DIAS=(148, 153),
          ING=(154, 173))

_t0 = time.time()


def log(msg=""):
    print(msg, flush=True)


def corte(par):
    a, b = par
    return slice(a - 1, b)


def detectar_salto(ruta, lrecl):
    """Devuelve el ancho real de registro (lrecl, +1 o +2 segun salto)."""
    with open(ruta, "rb") as fh:
        trozo = fh.read(min((lrecl + 2) * 400, 4_000_000))
    for extra, nombre in ((2, "CRLF"), (1, "LF"), (0, "sin salto")):
        ancho = lrecl + extra
        if len(trozo) < ancho * 3:
            continue
        ok = True
        for i in (1, 2, 3):
            reg = trozo[ancho * (i - 1):ancho * i]
            if len(reg) < ancho:
                ok = False
                break
            if extra == 2 and reg[-2:] != b"\r\n":
                ok = False
            if extra == 1 and reg[-1:] not in (b"\n", b"\r"):
                ok = False
        if ok:
            return ancho, nombre
    return lrecl, "sin salto (asumido)"


def registros(ruta, lrecl, bloque=64 * 1024 * 1024):
    ancho, nombre = detectar_salto(ruta, lrecl)
    with open(ruta, "rb") as fh:
        resto = b""
        while True:
            b = fh.read(bloque)
            if not b:
                break
            b = resto + b
            n = len(b) // ancho
            for i in range(n):
                yield b[i * ancho:i * ancho + lrecl]
            resto = b[n * ancho:]


def ent(b):
    t = b.strip()
    if not t:
        return None
    try:
        return int(t)
    except ValueError:
        return None


def num2d(b):
    t = b.strip()
    if not t:
        return None
    try:
        return int(t) / 100.0
    except ValueError:
        try:
            return float(t.replace(b",", b"."))
        except ValueError:
            return None


def localizar(carpeta, patrones):
    """Busca por ORDEN DE PRIORIDAD de patrones (el primero que exista gana),
    para no confundir vivhab2016.txt con VIVHAB2023.TXT cuando la carpeta
    contiene los dos anios del modulo."""
    ficheros = [p for p in sorted(carpeta.iterdir())
                if p.is_file() and p.name.lower().endswith(".txt")]
    for pat in patrones:
        for p in ficheros:
            if pat in p.name.lower():
                return p
    return None


def main_una():
    ap = argparse.ArgumentParser()
    ap.add_argument("--datos", default=RUTA_DATOS_DEFECTO)
    ap.add_argument("--modulo", default=None,
                    help="carpeta con VIVHAB2023/INM_PR2023/INM_CARACT2023 "
                         "(por defecto, la carpeta panel_inmob_txt del modulo)")
    ap.add_argument("--con-fichero8", action="store_true",
                    help="(por defecto YA activado en 2023) dias por RC del fichero 8")
    ap.add_argument("--sin-fichero8", action="store_true",
                    help="omite el paso 5 (dias por RC del fichero 8)")
    ap.add_argument("--anio", type=int, default=None, choices=(2016, 2023),
                    help="ejercicio (auto: segun el panel presente en --datos)")
    ap.add_argument("--solo-esta-carpeta", action="store_true",
                    help="no buscar automaticamente otros ejercicios")
    args = ap.parse_args()

    carpeta = Path(args.datos)
    if args.modulo:
        cmod = Path(args.modulo)
    elif Path(RUTA_MODULO_DEFECTO).is_dir():
        cmod = Path(RUTA_MODULO_DEFECTO)
    else:
        cmod = carpeta
    if not carpeta.is_dir():
        sys.exit(f"ERROR: no existe la carpeta de datos {carpeta}")

    anio = args.anio
    if anio is None:
        anio = 2023 if (carpeta / DISENOS[2023]["panel"]).exists() else 2016
    D = DISENOS[anio]
    global VIVHAB, INM_PR, INM_CA
    VIVHAB, INM_PR, INM_CA = D["VIVHAB"], D["INM_PR"], D["INM_CA"]
    log(f"Ejercicio: {anio}")
    panel_csv = carpeta / D["panel"]
    if not panel_csv.exists():
        sys.exit(f"ERROR: no encuentro {panel_csv}. Ejecuta antes el 01b.")

    f_vh = localizar(cmod, D["pats"]["vh"])
    f_pr = localizar(cmod, D["pats"]["pr"])
    f_ca = localizar(cmod, D["pats"]["ca"])
    log("Ficheros del modulo inmobiliario:")
    log(f"  VIVHAB    : {f_vh if f_vh else 'NO ENCONTRADO'}")
    log(f"  INM_PR    : {f_pr if f_pr else 'NO ENCONTRADO (opcional)'}")
    log(f"  INM_CARACT: {f_ca if f_ca else 'NO ENCONTRADO (opcional)'}")
    if f_vh is None and f_ca is None:
        sys.exit("ERROR: no encuentro ningun fichero del modulo en " + str(cmod)
                 + ". Indica la carpeta con --modulo.")

    # ---- 1) RCs e IDENPERs del panel de arrendamientos ----------------------
    log("\n1) Leyendo el panel de arrendamientos...")
    with open(panel_csv, encoding="utf-8", errors="replace", newline="") as fh:
        rd = csv.DictReader(fh, delimiter=";")
        cab = rd.fieldnames or []
        c_rc = next((c for c in cab if c.upper() in
                     ("REFCAT", "RC_ANONIMA", "REFCAT_ANONIMA")), None)
        c_id = next((c for c in cab if c.upper() == "IDENPER"), None)
        if c_rc is None or c_id is None:
            sys.exit("ERROR: el panel no tiene columnas REFCAT/IDENPER.")
        filas = list(rd)
    rcs = set()
    titulares_panel = {}
    sin_rc = 0
    for f in filas:
        rc = (f.get(c_rc) or "").strip()
        idp = (f.get(c_id) or "").strip()
        if not rc or not rc.strip("0"):
            sin_rc += 1
            continue
        rc = rc.lstrip("0") or "0"
        rcs.add(rc)
        titulares_panel.setdefault(rc, set()).add(idp)
    log(f"   {len(filas):,} inmuebles | {len(rcs):,} RC distintas | "
        f"{sin_rc:,} sin RC".replace(",", "."))

    # ---- 2-4) MODULO: se intenta con los ficheros del ejercicio y, si la
    # tasa de cruce es casi nula (los identificadores anonimos de ese anio no
    # casan con los del panel), se REINTENTA con los del otro ejercicio: en
    # algunas entregas la RC_ANONIMA es comun a toda la entrega y no por anio.
    def pasada(fvh, fpr, fca, dis, etiqueta):
        VH, PR, CA = dis["VIVHAB"], dis["INM_PR"], dis["INM_CA"]
        vh_d, urb_d, tit_d, car_d, vcpr_d = {}, {}, {}, {}, {}
        if fvh is not None:
            log(f"\n2) VIVHAB ({etiqueta})...")
            n = h = 0
            for r in registros(fvh, VH["lrecl"]):
                n += 1
                rc = r[slice(VH["RC"][0] - 1, VH["RC"][1])].strip().lstrip(b"0").decode() or "0"
                if rc not in rcs:
                    continue
                h += 1
                idp = r[slice(VH["IDENPER"][0] - 1, VH["IDENPER"][1])].strip().decode()
                tipo = r[slice(VH["TIPO"][0] - 1, VH["TIPO"][1])].strip().decode()
                ucl = r[slice(VH["URBACLAVE"][0] - 1, VH["URBACLAVE"][1])].strip().decode()
                vh_d.setdefault(rc, []).append((idp, tipo))
                if ucl:
                    urb_d[rc] = ucl
            log(f"   {n:,} registros | {h:,} en RC del panel | {len(vh_d):,} RC con alguien domiciliado".replace(",", "."))
        if fpr is not None:
            log(f"\n3) INM_PR ({etiqueta})...")
            n = h = 0
            for r in registros(fpr, PR["lrecl"]):
                n += 1
                rc = r[slice(PR["RC"][0] - 1, PR["RC"][1])].strip().lstrip(b"0").decode() or "0"
                if rc not in rcs:
                    continue
                h += 1
                tit_d.setdefault(rc, set()).add(
                    r[slice(PR["IDENPER"][0] - 1, PR["IDENPER"][1])].strip().decode())
                vcp = num2d(r[slice(PR["VALORC"][0] - 1, PR["VALORC"][1])])
                if vcp:
                    vcpr_d[rc] = max(vcpr_d.get(rc, 0.0), vcp)
            log(f"   {n:,} registros | {h:,} en RC del panel".replace(",", "."))
        if fca is not None:
            log(f"\n4) INM_CARACT ({etiqueta})...")
            n = h = 0
            for r in registros(fca, CA["lrecl"]):
                n += 1
                rc = r[slice(CA["RC"][0] - 1, CA["RC"][1])].strip().lstrip(b"0").decode() or "0"
                if rc not in rcs:
                    continue
                h += 1
                car_d[rc] = dict(
                    PROV=r[slice(CA["PROV"][0] - 1, CA["PROV"][1])].strip().decode(),
                    MUN=r[slice(CA["MUN"][0] - 1, CA["MUN"][1])].strip().decode(),
                    VIV=ent(r[slice(CA["VIV"][0] - 1, CA["VIV"][1])]),
                    VIV_M=num2d(r[slice(CA["VIV_M"][0] - 1, CA["VIV_M"][1])]),
                    ANCONS=ent(r[slice(CA["ANCONS"][0] - 1, CA["ANCONS"][1])]),
                    VALCAT=num2d(r[slice(CA["VALCAT"][0] - 1, CA["VALCAT"][1])]))
            log(f"   {n:,} registros | {h:,} RC del panel con caracteristicas".replace(",", "."))
        return vh_d, urb_d, tit_d, car_d, vcpr_d

    vivhab, urbaclave, titulares_cat, caract, vc_pr = pasada(
        f_vh, f_pr, f_ca, D, f"ejercicio {anio}")
    tasa = len(caract) / max(len(rcs), 1)
    if tasa < 0.05:
        alt = 2016 if anio == 2023 else 2023
        Dalt = DISENOS[alt]
        a_vh = localizar(cmod, Dalt["pats"]["vh"])
        a_pr = localizar(cmod, Dalt["pats"]["pr"])
        a_ca = localizar(cmod, Dalt["pats"]["ca"])
        if a_ca is not None:
            log(f"\n   *** La tasa de cruce con el modulo {anio} es casi nula "
                f"({100*tasa:.1f}%).")
            log(f"   *** REINTENTO con el modulo de {alt}: en algunas entregas la")
            log(f"   *** RC_ANONIMA es comun a toda la entrega, no por ejercicio.")
            v2, u2, t2, c2, p2 = pasada(a_vh, a_pr, a_ca, Dalt, f"ejercicio {alt}")
            if len(c2) > len(caract):
                log(f"\n   -> El modulo de {alt} cruza mejor "
                    f"({len(c2):,} vs {len(caract):,} RC): se usa ese."
                    .replace(",", "."))
                vivhab, urbaclave, titulares_cat, caract = v2, u2, t2, c2
                vc_pr = p2
                f_vh, f_pr, f_ca = a_vh, a_pr, a_ca

    # ---- 5) (opcional) dias por RC del fichero 8 completo -------------------
    dias_rc = {}
    # El paso 5 (dias censales por RC) va ACTIVADO POR DEFECTO en 2023: es la
    # variable que agrega la c.101 de todos los declarantes de cada inmueble.
    if not args.sin_fichero8 and anio == 2023:
        args.con_fichero8 = True
    if args.con_fichero8 and anio != 2023:
        log("\n5) AVISO: --con-fichero8 solo disponible para 2023 (el diseno "
            "del fichero de 2016 difiere); omitido.")
        args.con_fichero8 = False
    if args.con_fichero8:
        f8 = localizar(carpeta, ["8_irpf2023_rrii", "irpf2023_rrii", "rrii"])
        if f8 is None:
            log("\n5) AVISO: no encuentro el fichero 8; omito los dias por RC.")
        else:
            log(f"\n5) Fichero 8 completo (dias por RC): {f8.name} — puede "
                "tardar varios minutos...")
            n = 0
            for r in registros(f8, F8["lrecl"]):
                n += 1
                if n % 1_000_000 == 0:
                    log(f"   ... {n:,} registros ({int(time.time()-_t0)} s)"
                        .replace(",", "."))
                rc = r[corte(F8["REFCAT"])].strip().lstrip(b"0").decode() or "0"
                if rc not in rcs:
                    continue
                d = ent(r[corte(F8["DIAS"])]) or 0
                idp = r[corte(F8["IDENPER"])].strip().decode()
                reg = dias_rc.setdefault(rc, [0, set()])
                reg[0] += d
                reg[1].add(idp)
            log(f"   {n:,} registros del fichero 8 recorridos"
                .replace(",", "."))

    # ---- 6) escribir el panel enriquecido -----------------------------------
    log("\n6) Escribiendo el panel enriquecido...")
    nuevas = ["INQ_DOMICILIADO", "N_VIVHAB_RC", "N_VIVHAB_AJENOS",
              "URBACLAVE_RC", "VALCAT_PR", "PROV_INM", "MUN_INM", "VIV_RC",
              "VIV_METROS_RC", "ANCONS_RC", "VALCAT_RC"]
    if args.con_fichero8:
        nuevas += ["DIAS_RC", "NDECL_RC"]
    bak = panel_csv.with_suffix(".csv.bak")
    if not bak.exists():
        panel_csv.replace(bak)
        origen = bak
    else:
        origen = panel_csv.with_suffix(".csv.tmp")
        panel_csv.replace(origen)
    n_inq = n_car = 0
    with open(origen, encoding="utf-8", errors="replace", newline="") as fin, \
         open(panel_csv, "w", encoding="utf-8", newline="") as fout:
        rd = csv.DictReader(fin, delimiter=";")
        cab_out = list(rd.fieldnames) + [c for c in nuevas
                                         if c not in rd.fieldnames]
        wr = csv.DictWriter(fout, fieldnames=cab_out, delimiter=";",
                            lineterminator="\n")
        wr.writeheader()
        for f in rd:
            rc = (f.get(c_rc) or "").strip().lstrip("0") or ""
            idp = (f.get(c_id) or "").strip()
            vh = vivhab.get(rc, [])
            tits = set(titulares_cat.get(rc, set()))
            tits |= titulares_panel.get(rc, set())
            ajenos = [p for p, t in vh if p not in tits]
            f["N_VIVHAB_RC"] = len(vh) if rc else ""
            f["N_VIVHAB_AJENOS"] = len(ajenos) if rc else ""
            f["INQ_DOMICILIADO"] = (1 if ajenos else 0) if rc else ""
            f["URBACLAVE_RC"] = urbaclave.get(rc, "")
            f["VALCAT_PR"] = (f"{vc_pr[rc]:.2f}" if rc in vc_pr else "")
            ca = caract.get(rc)
            f["PROV_INM"] = ca["PROV"] if ca else ""
            f["MUN_INM"] = ca["MUN"] if ca else ""
            f["VIV_RC"] = ca["VIV"] if ca and ca["VIV"] is not None else ""
            f["VIV_METROS_RC"] = (f"{ca['VIV_M']:.2f}"
                                  if ca and ca["VIV_M"] is not None else "")
            f["ANCONS_RC"] = ca["ANCONS"] if ca and ca["ANCONS"] else ""
            f["VALCAT_RC"] = (f"{ca['VALCAT']:.2f}"
                              if ca and ca["VALCAT"] is not None else "")
            if args.con_fichero8:
                reg = dias_rc.get(rc)
                f["DIAS_RC"] = min(reg[0], 365) if reg else ""
                f["NDECL_RC"] = len(reg[1]) if reg else ""
            if ajenos:
                n_inq += 1
            if ca:
                n_car += 1
            wr.writerow(f)
    if origen.suffix == ".tmp":
        origen.unlink()

    log("\n" + "=" * 74)
    log("  RESUMEN DEL CRUCE")
    log("=" * 74)
    log(f"  Inmuebles del panel                    : {len(filas):,}"
        .replace(",", "."))
    tasa_ca = 100 * n_car / max(len(filas), 1)
    log(f"  Con caracteristicas (INM_CARACT)       : {n_car:,}  ({tasa_ca:.1f}%)"
        .replace(",", "."))
    log(f"  Con INQUILINO DOMICILIADO (VIVHAB)     : {n_inq:,}"
        .replace(",", "."))
    n_vc = sum(1 for f in filas
               if (f.get(c_rc) or "").strip().lstrip("0") in vc_pr)
    log(f"  Con VALOR CATASTRAL (INM_PR, cobertura extra): {n_vc:,}  "
        f"({100*n_vc/max(len(filas),1):.1f}%)".replace(",", "."))
    if tasa_ca < 5 and f_ca is not None:
        log("\n  *** ATENCION: tasa de cruce casi nula. La RC_ANONIMA del")
        log("  *** modulo NO casa con la REFCAT del fichero 8: comprueba que")
        log("  *** el modulo sea del MISMO panel/entrega. No se puede cruzar.")
    log("\n  Columnas anadidas: " + ", ".join(nuevas))
    log("  Siguiente paso: source('02_informe_arrendadores.R') SIN tocar nada:")
    log("  detecta estas columnas solo (uso catastral, dias censales por RC y,")
    log("  en ejercicios sin valor catastral en el IRPF, el perimetro del modulo).")
    log(f"\nHecho en {int(time.time()-_t0)} s.")


def _autoejecutar():
    import io
    import traceback

    class _Tee(io.TextIOBase):
        def __init__(self, *d):
            self.d = d

        def write(self, t):
            for x in self.d:
                try:
                    x.write(t)
                    x.flush()
                except Exception:
                    pass
            return len(t)

    carpeta = None
    argv = sys.argv[1:]
    if "--datos" in argv:
        try:
            carpeta = Path(argv[argv.index("--datos") + 1])
        except Exception:
            carpeta = None
    if carpeta is None or not carpeta.is_dir():
        carpeta = Path(RUTA_DATOS_DEFECTO)
    fh = None
    ruta_log = None
    if carpeta.is_dir():
        ruta_log = carpeta / "enriquecer_inmuebles_log.txt"
        try:
            fh = open(ruta_log, "w", encoding="utf-8")
        except Exception:
            fh = None
    out = sys.stdout
    if fh is not None:
        sys.stdout = _Tee(out, fh)
        sys.stderr = _Tee(out, fh)
    try:
        main()
    except SystemExit as e:
        if e.code not in (0, None):
            print("\n" + "=" * 74)
            print("  EL SCRIPT SE HA DETENIDO. Motivo arriba.")
            print("=" * 74)
    except BaseException:
        print("\n" + "=" * 74)
        print("  ERROR INESPERADO:")
        print("=" * 74)
        traceback.print_exc()
    finally:
        if fh is not None:
            sys.stdout = out
            sys.stderr = out
            fh.close()
            print(f"\n(Registro completo en: {ruta_log})")
        try:
            input("\nPulsa Enter para cerrar...")
        except EOFError:
            pass


def main():
    """Cruza la carpeta indicada y, ademas, cualquier OTRO ejercicio del panel
    que encuentre junto a ella (p. ej. ...\\2016 al lanzarlo sobre ...\\2023).
    Asi, con una sola ejecucion quedan cruzados todos los anios y la serie
    temporal del informe puede perimetrarse con el mismo criterio."""
    argv0 = list(sys.argv)
    try:
        main_una()
    except SystemExit as e:
        if e.code not in (0, None):
            print("  (esta carpeta no se pudo cruzar; se buscan otros ejercicios)")
    if "--solo-esta-carpeta" in argv0:
        return
    base = None
    if "--datos" in argv0:
        try:
            base = Path(argv0[argv0.index("--datos") + 1])
        except Exception:
            base = None
    if base is None or not base.is_dir():
        base = Path(RUTA_DATOS_DEFECTO)
    if not base.is_dir() or not base.parent.is_dir():
        return
    hechas = {base.resolve()}
    for d in sorted(base.parent.iterdir()):
        if not d.is_dir() or d.resolve() in hechas:
            continue
        if not any((d / f"panel_arrendamientos_{a}.csv").is_file()
                   for a in (2016, 2023)):
            continue
        print("\n" + "=" * 74)
        print(f"  OTRO EJERCICIO DETECTADO: {d.name}  ->  cruzando tambien")
        print("=" * 74)
        argv = [argv0[0], "--datos", str(d), "--solo-esta-carpeta"]
        if "--modulo" in argv0:
            argv += ["--modulo", argv0[argv0.index("--modulo") + 1]]
        sys.argv = argv
        try:
            main_una()
        except SystemExit as e:
            if e.code not in (0, None):
                print(f"  (omitido {d.name}: revisa el mensaje anterior)")
        except Exception as exc:
            print(f"  (error en {d.name}: {exc})")
    sys.argv = argv0


if __name__ == "__main__":
    _autoejecutar()
