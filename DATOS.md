# Estructura de datos esperada

Los microdatos NO se distribuyen con este repositorio (licencia IEF). Estructura
de carpetas que esperan los scripts (nombres de fichero tal como los entrega el IEF):

```
TU_CARPETA/
├── 2023/
│   ├── _2_Renta2023.txt
│   ├── _8_IRPF2023_RRII.txt            (fichero 8, lrecl 1061)
│   └── (aquí se generan panel_arrendamientos_2023.csv y los logs)
├── 2016/
│   └── ... (equivalentes de 2016; el extractor del fichero 4 no se incluye)
└── MODULO_INMOBILIARIO/panel_inmob_txt/
    ├── VIVHAB2023.TXT      (lrecl 29)    ├── vivhab2016.txt      (lrecl 26)
    ├── INM_PR2023.TXT      (lrecl 61)    ├── Inm_pr2016.txt      (lrecl 62)
    └── INM_CARACT2023.TXT  (lrecl 82)    └── Inm_caract2016.txt  (lrecl 83)
```

Los diseños de registro oficiales (hojas Excel del IEF) documentan cada posición;
los scripts los llevan incorporados. El 01d verifica el cruce e imprime la tasa:
si sale ~0 %, estás cruzando ejercicios distintos o ficheros de otra entrega.
