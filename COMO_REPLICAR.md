# Cómo replicar estos resultados en 5 pasos (sin ser experto en el Panel)

Esta guía asume solo que sabes ejecutar un script de Python y uno de R.

**Paso 0 — Consigue los datos.** Solicita al IEF el *Panel de Hogares* (2016 y
2023) y su *Módulo Inmobiliario*. Coloca los ficheros como muestra
`docs/DATOS.md`. Este repositorio **no** incluye microdatos.

**Paso 1 — Di a los scripts dónde están tus datos.** Abre cada script y cambia
una sola variable (viene marcada arriba del todo): `RUTA_DATOS_DEFECTO` en los
`.py` (y `RUTA_MODULO_DEFECTO` en `01d`) y `RUTA_RAIZ` en el `.R`. También
puedes pasar `--datos "TU_RUTA"` por línea de comandos y no tocar nada.

**Paso 2 — Extrae el panel de arrendamientos.** Ejecuta
`01b_extraer_panel_sin_dependencias.py` (doble clic vale). Lee el fichero 8 del
IRPF (5 GB, en streaming) y crea `panel_arrendamientos_2023.csv`: una fila por
inmueble arrendado y declarante, con sus casillas clave.

**Paso 3 — Enriquece.** Ejecuta `01c_enriquecer_panel.py` (añade casillas
adicionales y la renta bruta del declarante) y después
`01d_cruzar_inmuebles.py` (cruza cada inmueble con el Módulo Inmobiliario:
uso catastral, superficie, ubicación real y días censales). Ambos detectan y
procesan **solos** todos los ejercicios que encuentren (2023 y 2016) y
escriben un log legible junto a los datos. Lee siempre el **RESUMEN** final:
las tasas de cruce te dicen si todo casó bien.

**Paso 4 — Genera el informe.** Desde R o RStudio:
`source("02_informe_arrendadores.R", encoding = "UTF-8")`. La consola imprime
un sello de versión y varios diagnósticos ([M5] perímetros, cobertura
catastral, % de la casilla 93...). El resultado es
`informe_arrendadores_vivienda.xlsx` con una hoja por módulo (M1–M5), la
validación frente a la AEAT, notas, metadatos y dos hojas de gráficos.

**Paso 5 — Comprueba que replicaste.** Abre la hoja `Validacion_AEAT`: los
ratios Panel/AEAT del ejercicio de referencia deben quedar entre 0,94 y 1,06
en todas las métricas. Si algo se desvía, revisa los logs de los pasos 2–3 (las
tasas de cruce) antes que el código.

*Todos los parámetros del criterio están al principio del script de R, con su
efecto explicado en comentario. La configuración por defecto es la validada:
no cambies nada para replicar.*
