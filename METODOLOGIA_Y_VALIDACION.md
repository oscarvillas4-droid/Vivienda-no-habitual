# Metodología, decisiones y validación

Documento completo del ejercicio: qué fuentes se usan, **qué decisión se tomó
en cada bifurcación y por qué**, cómo se validó el resultado y qué límites
tiene. Las decisiones se numeran (D1, D2…) para poder citarlas.

---

## 1. Objetivo y fuentes

**Objetivo.** Reproducir, con microdatos del Panel de Hogares IEF-AEAT, la
estadística oficial *Vivienda en alquiler declarada en el IRPF* (AEAT), que
clasifica las viviendas arrendadas en **habituales** (constituyen la vivienda
habitual del inquilino) y **no habituales** (el resto), y explotar después el
microdato para responder preguntas que la estadística publicada no cubre.

**Fuentes.**

| Fuente | Contenido | Papel |
|---|---|---|
| Panel de Hogares IEF-AEAT, fichero 8 | Un registro por inmueble arrendado y declarante, con la referencia catastral seudonimizada | Universo de trabajo |
| Panel, fichero 2 (Renta) | Renta bruta del declarante y sus componentes | Módulo 4 (renta) |
| Módulo Inmobiliario del Panel | `INM_CARACT` (características censales por referencia: provincia, municipio, nº de viviendas, superficie, año, valor catastral), `INM_PR` (titularidades y valor catastral), `VIVHAB` (vivienda habitual de cada persona del panel) | Perímetro de vivienda, ubicación real, superficie |
| Estadística AEAT 2023, Bloque I | 2.409.689 viviendas / 657 €/mes / 339 días (habitual); 309.479 / 1.361 € / 271 días (no habitual) | Ancla de validación |

El panel es una **muestra** (~5 %) de declarantes, elevada con `FACTORCAL`. La
AEAT trabaja con el censo completo: esa asimetría explica varias decisiones.

---

## 2. Decisiones de construcción del universo

**D1. Unidad de análisis = vivienda entera.** Cada inmueble se eleva por
`participación × FACTORCAL`, de modo que un piso al 50 % entre dos cotitulares
cuenta media vivienda por cada uno y una sola en el total. *Motivo:* es la
unidad del Bloque I de la AEAT. *Alternativa descartada:* ponderar por días
ocupados (lógica del Bloque II, que descuadra los recuentos).

**D2. Universo de partida.** Inmuebles urbanos con ingresos por arrendamiento
(casilla 102 > 0) y situación de casilla 65 = 1 (territorio común, con
referencia catastral). *Se excluyen:* los afectos íntegramente a actividad
económica (no forman parte de la estadística AEAT) y los accesorios sin
ingresos propios (plazas y trasteros alquilados junto con la vivienda, que no
son un arrendamiento independiente).

**D3. País Vasco y Navarra quedan fuera**, igual que en la estadística oficial,
por sus regímenes forales.

---

## 3. Decisiones de clasificación

**D4. Habitual = casillas 100/150, sin filtros añadidos.** La reducción del
artículo 23.2 LIRPF solo cabe legalmente cuando el destino del inmueble es la
vivienda habitual del arrendatario, de modo que la casilla *es* el criterio.
*Validación:* 2.423.187 viviendas elevadas frente a 2.409.689 oficiales →
**ratio 1,006**. Se probaron filtros adicionales sobre este segmento y todos
empeoraban el ajuste: descartados.

**D5. Fuera de la tabla.** No entran en ninguna de las dos columnas: los
inmuebles con uso propio del titular durante el año (casilla 76 > 0: la FAQ de
la AEAT los trata como vivienda habitual con parte arrendada), los registros
sin días declarados y el bloque de **arrendamiento anual de bajo importe sin
reducción**. Este último agrupa dos realidades que el fichero no permite
separar entre sí y que la AEAT tampoco cuenta como no habitual: (a) garajes,
trasteros y locales alquilados todo el año con contrato propio, que la AEAT
aparta con el cruce catastral; y (b) pisos que en la práctica son la residencia
habitual de su inquilino aunque el arrendador no consignara la reducción, que
la AEAT reclasifica como habituales gracias al domicilio fiscal del
arrendatario.

**D6. No habitual = el resto del arrendamiento sin reducción, tras superar
candados de plausibilidad de vivienda.** Los candados existen porque el fichero
8 mezcla viviendas con locales, naves, garajes y trasteros, y la AEAT los
separa con Catastro. En orden de prioridad:

1. **Prueba catastral.** Si el módulo informa uso no residencial de la
   referencia (clave distinta de «V»), el inmueble queda fuera aunque tenga
   valor catastral declarado. Si informa uso residencial o número de viviendas
   ≥ 1, se acepta, con un mínimo de superficie por vivienda que aparta
   trasteros y cuartos con referencia propia. Si el módulo no cubre esa
   referencia (~5 %), se recurre al valor catastral del IRPF dentro de la banda
   residencial de su provincia.
2. **Ingreso anual mínimo de vivienda**, que descarta importes incompatibles
   con el alquiler de una vivienda completa.
3. **Precio por metro cuadrado dentro de una banda de plausibilidad** respecto
   al €/m² provincial publicado por la AEAT.

*Validación:* 301.815–312.918 viviendas según corrida → **ratio 0,98–1,01**.

**D7. Decisiones probadas y revertidas.** Quedan como conmutadores comentados
en el script, con su efecto medido, para trazabilidad:

| Experimento | Efecto medido | Decisión |
|---|---|---|
| Excluir del no habitual los contratos antiguos (casilla 93) | ≈ 6.000 viviendas | Revertido: la casilla apenas se cumplimenta (el script mide el % en cada corrida y lo publica en `Notas`) |
| Excluir los inmuebles con inquilino confirmado en `VIVHAB` | ≈ 1.000 viviendas | Revertido: solo observa inquilinos que cayeron en la muestra |
| Excluir el arrendamiento intensivo de más de 330 días | Corregía Madrid, pero amputaba identificación legítima | Revertido |
| Calcular los patrones de precio por provincia **del inmueble** en vez de por residencia del declarante | Empeoraba: Madrid 1,52 → 1,89; mediana provincial 0,98 → 0,90 | Revertido |
| Criterio del no habitual por uso catastral puro | Días 0,94 pero alquiler 0,63 | Reservado como análisis de sensibilidad |

**D8. Uni y multiarrendador se determinan por la cartera TOTAL de viviendas en
alquiler del declarante, con independencia del régimen:** uniarrendador = 1
vivienda arrendada; multiarrendador = 2 o más (desglose 2 / 3 / 4 / 5-9 /
10 o más). El análisis se refiere a los declarantes con al menos una vivienda
no habitual. *Motivo:* la pregunta de investigación es quién explota el
alquiler, y un propietario con un piso habitual y un apartamento de temporada
es un multiarrendador aunque solo uno sea no habitual. Los recuentos de
viviendas se elevan por participación, de forma que **uni + multi suman
exactamente** el total de la hoja de validación. El criterio es **idéntico en
los módulos 3, 4 y 5**: que el dato de M3 coincida con el del año
correspondiente de M5 es la comprobación de que no hay desajuste.

---

## 4. Decisiones de medición

**D9. Alquiler medio mensual = ingresos íntegros del ejercicio al 100 % de
titularidad ÷ 12, media ponderada por vivienda.** Es la definición que replica
las dos anclas a la vez (0,98 y 1,06). *Alternativa descartada:* elevar cada
vivienda a año completo según sus días ocupados, que da 1,05 y 1,62. Esta
métrica es **única en todo el informe** —tablas, gráficos y validación usan la
misma— y la variante elevada solo interviene, internamente, en los indicadores
de intensidad que sirven para clasificar.

**D10. Días = días censales por referencia catastral.** La casilla 101 se
agrega entre **todos** los declarantes de cada inmueble recorriendo el fichero
8 completo (paso 5 del script `01d`), con tope de 365. *Motivo:* la AEAT
califica cada inmueble con el conjunto de las declaraciones; leer solo la del
declarante muestreado pierde los días que declaran cotitulares no muestreados.
*Efecto:* el ratio de días del no habitual pasa de 0,83 a **0,94**, y a nivel
provincial de 0,92 a 0,99 en las celdas grandes.

**D11. Ingresos anuales por declarante** (módulo 2): suma de la parte de
ingresos del declarante (casilla 102, ya prorrateada) en todas sus viviendas de
cada modalidad, promediada con `FACTORCAL`.

**D12. Territorio = ubicación real del inmueble** (`PROV_INM` / `MUN_INM` del
módulo) en los contrastes provinciales y autonómicos, no la residencia del
declarante. *Motivo:* es el criterio de la tabla oficial. La residencia se
conserva solo en las series municipales, donde el módulo no siempre cubre.

---

## 5. Validación

| Nivel | Resultado |
|---|---|
| Nacional | 8/8 métricas entre 0,94 y 1,06 (recuentos 1,01 y 0,98; alquiler 0,98 y 1,06; días 1,00 y 0,94) |
| Provincial (por ubicación) | Días 0,92–0,99 en todas las celdas grandes; recuentos con mediana ≈ 0,98 |
| Autonómico | Las cinco comunidades con más peso, dentro de ±10 % en ambas variables |
| Por casero (métrica difundida en prensa) | Habitual 1,06 |

El informe genera su propia auditoría en cada corrida: `IC_bootstrap_prov`
(intervalos al 95 % por remuestreo de declarantes, con veredicto automático
**VARIANZA** o **SESGO** según caiga o no el valor oficial dentro del
intervalo), `Mapa_divergencias` (origen probable de cada desviación) y
`Prov_ubicacion_calado` (pesos post-estratificados a los marginales oficiales,
para explotación posterior con geografía coherente con la publicada).

---

## 6. Límites y por qué son límites

**L1. El domicilio fiscal del arrendatario no es observable.** La AEAT
clasifica cruzando la referencia catastral real con el censo de domicilios. En
el panel, los NIF de arrendatarios (casillas 91/94/97) fueron suprimidos en la
anonimización —verificado en los diseños de registro—, la casilla 93 apenas se
cumplimenta y `VIVHAB` solo observa inquilinos muestreados. *Consecuencia:* el
arrendamiento anual con inquilino residente no declarado como habitual no puede
separarse; se concentra en Madrid y Barcelona, cuyos recuentos quedan por
encima del oficial y figuran certificados como sesgo por el bootstrap. Todas
las alternativas se probaron y midieron (D7).

**L2. Métrica del alquiler en la desagregación territorial.** El dato oficial
eleva ingresos a año completo; la lente ingresos/12 infrarrepresenta la
estancia corta (costa). El informe publica **tres lentes** por provincia
(ingresos/12, elevada por vivienda y elevación agregada) para acotar la
comparación, sin alterar ninguna clasificación.

**L3. Error muestral en celdas pequeñas.** Un panel del ~5 % no soporta
precisión de ±4 % por provincia pequeña ni por municipio; el bootstrap lo
cuantifica y el nivel autonómico es el más fino donde exigir la banda estricta
tiene sentido estadístico.

**L4. Cobertura catastral y comparabilidad de la serie.** El cruce alcanza
≈ 97 % en el ejercicio de referencia y ≈ 86 % en 2016 (el panel de 2016 se
cruza con la foto catastral de la entrega de 2023: siete años de rotación
—ventas fuera del panel, divisiones horizontales que reasignan referencia,
titulares que abandonan el panel—, más rústicos y BICE sin ficha de viviendas).
*Hallazgo:* en esta entrega el seudónimo catastral es común a toda la entrega y
no por ejercicio —los ficheros del módulo de 2016 no cruzan con el panel de
2016 (0 %) y los de 2023 sí (81 %)—; el script `01d` lo detecta y reintenta
solo. Lo relevante no es el porcentaje sino su **asimetría**: el año con menos
cobertura infravalora sus niveles en esa proporción, por lo que M5 publica
`cobertura_catastral_pct`, una columna de nivel ajustado por cobertura como
horquilla, y el script avisa en consola cuando la diferencia supera 5 puntos.
**Recomendación:** comparar 2016 y 2023 en ratios y estructura, no en niveles
absolutos. Además, el perímetro de un ejercicio anterior hereda los cambios de
uso posteriores a su año.

**L5. Fuera del alcance de la fuente:** propietarios no residentes,
arrendamiento turístico ejercido como actividad económica y sociedades, que no
tributan en este fichero.

---

## 7. Reproducibilidad

Cada corrida imprime un **sello de versión** y sus diagnósticos (criterio
aplicado, módulos detectados, perímetro de cada ejercicio, cobertura, % de
cumplimentación de la casilla 93). Cada exclusión tiene contador en las hojas
de embudo y cada experimento quedó como conmutador comentado con su efecto
medido. La configuración por defecto es la validada en este documento: para
replicar no hace falta cambiar ningún parámetro.
