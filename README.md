# Réplica de la Estadística de Viviendas en Alquiler del IRPF (AEAT) con el Panel de Hogares IEF-AEAT

Ingeniería inversa, con microdatos del **Panel de Hogares IEF-AEAT** (ejercicios 2016 y 2023)
y su **Módulo Inmobiliario**, de la estadística oficial
[*Vivienda en alquiler declarada en el IRPF*](https://sede.agenciatributaria.gob.es/Sede/estadisticas/estadisticas-impuesto/estadistica-viviendas-declaradas-irpf.html)
de la Agencia Tributaria: clasificación de las viviendas arrendadas en **habituales**
(vivienda habitual del inquilino) y **no habituales** (otras finalidades), con sus alquileres, días de arrendamiento y desagregación territorial.

**Validación frente al dato oficial 2023 (ratios Panel/AEAT):**

| Segmento | Viviendas | Alquiler medio | Días (censales) |
|---|---|---|---|
| Habitual (2.409.689 AEAT) | **1,01** | **0,98** | **1,00–1,02** |
| No habitual (309.479 AEAT) | **0,98** | **1,06** | **0,94** |

El informe generado incluye además contraste provincial y por CCAA **por ubicación
real del inmueble**, intervalos de confianza bootstrap por provincia, pesos calados
a los marginales AEAT y un mapa de divergencias autoexplicado. Las limitaciones
conocidas (Madrid/Barcelona) están documentadas en `docs/METODOLOGIA_Y_VALIDACION.md`.

---

## ⚠️ Datos: qué NO contiene este repositorio

**Este repositorio contiene solo código y documentación.** Los microdatos del Panel
de Hogares IEF-AEAT y de su Módulo Inmobiliario están sujetos a licencia de uso del
Instituto de Estudios Fiscales y **no se redistribuyen aquí**. Para replicar:

1. Solicita el Panel de Hogares (ejercicios 2016 y/o 2023) al IEF:
   <https://www.ief.es> → Estadísticas / Panel de Hogares.
2. Solicita/descarga el Módulo Inmobiliario 2016–2023 (Documento de Trabajo IEF 4/2025).
3. Coloca los ficheros como se indica en `docs/DATOS.md`.

## Requisitos

* **Python ≥ 3.8** — sin dependencias externas (solo biblioteca estándar).
* **R ≥ 4.0** con el paquete **`data.table`** (obligatorio) y, opcionalmente,
  **`openxlsx`** o **`writexl`** (si faltan, el informe se vuelca a CSV, uno por hoja).
* Espacio en disco: los ficheros de texto del panel ocupan varios GB; los scripts
  los leen en streaming (no cargan todo en memoria).

## 🔧 Configura TUS rutas (lo único que debes cambiar)

Cada script trae las rutas del autor como valor por defecto. Cámbialas por las tuyas
(o pásalas por línea de comandos donde exista la opción):

| Script | Variable | Línea aprox. |
|---|---|---|
| `scripts/01b_extraer_panel_sin_dependencias.py` | `RUTA_DATOS_DEFECTO` | ~66 |
| `scripts/01c_enriquecer_panel.py` | `RUTA_DATOS_DEFECTO` | ~77 |
| `scripts/01d_cruzar_inmuebles.py` | `RUTA_DATOS_DEFECTO` y `RUTA_MODULO_DEFECTO` | ~42 y ~51 |
| `scripts/02_informe_arrendadores.R` | `RUTA_RAIZ` | ~66 |

En Windows, escribe las rutas con `r"..."` en Python (tal como vienen) y con
`\\` dobles en R (tal como viene). Los `.py` también aceptan `--datos "TU_RUTA"`
(y el 01d, `--modulo "TU_RUTA_DEL_MODULO"`), que **prevalece** sobre el valor por defecto.

## ▶️ Orden de ejecución (pipeline)

```text
fichero 8 IRPF (texto plano, ~5 GB)          módulo inmobiliario (3 TXT)
        │                                              │
   [01b] extraer  ──►  panel_arrendamientos_2023.csv   │
        │                                              │
   [01c] enriquecer (casillas extra + renta)           │
        │                                              │
   [01d] cruzar módulo por RC anonimizada  ◄───────────┘
        │        (añade uso catastral, m², ubicación, inquilino, días censales)
        ▼
   [02]  informe en R  ──►  informe_arrendadores_vivienda.xlsx (o CSVs)
```

> **Serie temporal:** `01c` y `01d` procesan **automáticamente todos los
> ejercicios** que encuentren junto a la carpeta indicada (por ejemplo, al
> lanzarlos sobre `...\2023` cruzan también `...\2016`). Sin el cruce de 2016
> con su módulo inmobiliario, ese año no puede perimetrarse igual que 2023 y el
> informe deja sus niveles en blanco advirtiéndolo. Si los identificadores de
> un año no casan con los ficheros de su propio módulo, `01d` reintenta solo con
> los del otro ejercicio (el seudónimo catastral suele ser común a la entrega).

```bash
# 1) Extraer el panel de arrendamientos del fichero 8 (una vez por ejercicio)
python scripts/01b_extraer_panel_sin_dependencias.py --datos "TU_CARPETA\2023"

# 2) Enriquecer con casillas adicionales del fichero 8 y renta del fichero 2
python scripts/01c_enriquecer_panel.py --datos "TU_CARPETA\2023"

# 3) Cruzar con el Módulo Inmobiliario (incluye por defecto el paso 5:
#    días censales por referencia catastral; tarda varios minutos)
python scripts/01d_cruzar_inmuebles.py --datos "TU_CARPETA\2023" --modulo "TU_CARPETA_MODULO"

# 4) Generar el informe (desde R o RStudio)
Rscript -e 'source("scripts/02_informe_arrendadores.R", encoding="UTF-8")'
```

Cada `.py` puede ejecutarse también con doble clic: escribe un `*_log.txt` junto a
los datos y se pausa al final para que leas el resumen. El script de R imprime al
arrancar un **sello de versión** y los diagnósticos de qué criterio y qué módulos
ha detectado: compruébalos siempre antes de interpretar resultados.

Para 2016: el 01c y el 01d detectan el ejercicio automáticamente (o `--anio 2016`);
el extractor equivalente al 01b para el fichero 4 de 2016 no se incluye (formato
propio del autor); el 02 de R procesa ambos ejercicios si encuentra sus CSV.

## 📊 Qué produce el informe (una hoja por módulo del diseño de investigación)

Con `SALIDA_MODULAR <- TRUE` (por defecto), el Excel contiene **exactamente**:

* `M1_Oferta_territorial` — viviendas en **vivienda habitual** y **vivienda no
  habitual** por provincia (ubicación del inmueble) y municipio, ratio entre
  modalidades y proporción de cada una sobre el parque arrendado.
* `M2_Diferencial_precio` — ingresos íntegros anuales medios **por declarante**
  en cada modalidad, días de ocupación medios (censales) y diferencial
  porcentual, por territorio.
* `M3_Estructura_propiedad` — uniarrendadores (1 vivienda no habitual) y
  multiarrendadores (2 / 3 / 4 / 5-9 / 10 o más): declarantes, viviendas
  acumuladas y proporción de ingresos que concentra cada categoría, por territorio.
* `M4_Renta_arrendadores` — renta bruta media de los declarantes por modalidad,
  tramo de renta y territorio; distribución uni/multi; peso de los ingresos por
  alquiler sobre la renta total.
* `M5_Evolucion_2016_2023` — series por modalidad: viviendas, ingresos medios
  por declarante, diferencial, uni/multiarrendadores y su cuota, y renta media.
* `Graficos` — un gráfico por variable de cada módulo, y `Graficos_extra` —
  hoja aparte con el retrato analítico de la vivienda no habitual
  (distribución por días alquilados, precio por día frente al alquiler
  habitual, municipios con más oferta, curva de concentración de ingresos,
  superficie media según Catastro y peso del alquiler por tramo de renta).
* `Validacion_AEAT`, `Notas`, `Metadatos` — la validación frente al dato
  oficial (tabla de ratios), las notas metodológicas (incluido el % de
  cumplimentación de la casilla 93 medido en la corrida) y la trazabilidad.

Con `SALIDA_MODULAR <- FALSE` se obtiene además toda la salida técnica
(embudos, calibración, bootstrap, mapas de divergencias…), pensada para auditoría.

## Parámetros y conmutadores del criterio (script de R, bloque inicial)

Todos los experimentos de la calibración quedaron **trazados como conmutadores**
documentados en el propio código (`NH_CRITERIO`, `USAR_ANTIG`, `NH_EXIGE_VC`,
`DIAS_LARGA_DURACION`, `ING_GARAJE_MAX`, `EURM2_BANDA`, `M2_MIN_VIVIENDA`,
`DIAS_TOPE_INTENSIVO`, `USAR_INQ_CONFIRMADO`…), con su efecto medido en comentario.
La configuración por defecto es la validada; cambia conmutadores solo si sabes
qué estás midiendo.

## 👀 ¿Solo quieres ver los resultados?

La carpeta **`resultados/`** contiene el informe ya generado
(`informe_arrendadores_vivienda.xlsx`) y los **24 gráficos en PNG**
(`resultados/graficos/`), para revisar el trabajo sin ejecutar nada. La hoja
`Metadatos` del Excel identifica la corrida (sello de versión, fecha,
parámetros y tasas de cruce).

## ¿Nuevo con el Panel de Hogares? Empieza aquí

Lee **`docs/COMO_REPLICAR.md`**: cinco pasos en lenguaje llano, sin asumir
experiencia previa con el panel. Cada script escribe además un log legible con
un RESUMEN final que te dice si el paso salió bien.

## Metodología, validación y limitaciones

Léelo antes de usar los resultados: **`docs/METODOLOGIA_Y_VALIDACION.md`** —
criterio de clasificación casilla a casilla, verificación empírica frente al dato
oficial, y las limitaciones estructurales de la fuente (en particular, por qué el
domicilio fiscal del arrendatario no es observable en el panel y qué implica para
Madrid y Barcelona).

## 🚀 Cómo publicar este repositorio en GitHub

**Opción A — desde el navegador (sin instalar nada):**

1. Entra en <https://github.com> → botón verde **New** (o *New repository*).
2. Nombre: `panel-arrendadores-irpf`. Visibilidad **Public**. **No** marques
   "Add a README file" (ya viene uno). → **Create repository**.
3. Descomprime el paquete en tu equipo. En la página del repo recién creado:
   **Add file → Upload files** y arrastra *el contenido* de la carpeta
   (`README.md`, `LICENSE`, `CITATION.cff`, `.gitignore`, y las carpetas
   `scripts/` y `docs/`) — no la carpeta comprimida.
4. Mensaje de commit: `v1.0.0 - pipeline completo y documentación` →
   **Commit changes**.
5. **Releases → Create a new release** → *Choose a tag* → escribe `v1.0.0` →
   *Publish release*. Eso congela una versión citable.

**Opción B — con Git instalado (línea de comandos):**

```bash
cd panel-arrendadores-irpf
git init
git add .
git commit -m "v1.0.0 - pipeline completo y documentación"
git branch -M main
git remote add origin https://github.com/TU_USUARIO/panel-arrendadores-irpf.git
git push -u origin main
git tag v1.0.0 && git push --tags
```

**Antes de publicar, comprueba:**

* Has puesto tu nombre en `LICENSE` y en `CITATION.cff` (marcados `[TU NOMBRE]`).
* En el repositorio **solo** hay archivos `.py`, `.R` y `.md` — ningún microdato
  ni CSV intermedio (el `.gitignore` los bloquea, pero revísalo a ojo).
* Si subes resultados agregados, ponlos en una carpeta `resultados/` indicando
  la fecha de la corrida y el sello de versión que imprime el script en consola.

**Opcional (recomendable para uso académico):** conecta el repositorio a
[Zenodo](https://zenodo.org) (*Settings → GitHub → activa el repo*); cada
*release* generará un DOI citable.

## Licencia y cita

Código bajo licencia **MIT** (`LICENSE`). Los microdatos pertenecen al IEF/AEAT y
se rigen por su propia licencia. Si usas este trabajo, cita el repositorio
(`CITATION.cff`) y la fuente de datos: *Panel de Hogares IEF-AEAT* y *Estadística
de vivienda en alquiler declarada en el IRPF (AEAT)*.

*Los resultados y opiniones derivados del uso de estos scripts son responsabilidad
de quien los ejecuta y no representan posición institucional alguna.*
