

library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(DT)       # Per la visualizzazione dei dati in formato tabellare
library(lubridate)
library(ggplot2)
library(plotly)
library(tidyverse)
library(scales)
library(purrr)
library(htmltools)
library(grid)
library(shinyjs)
library(RColorBrewer)
library(shinyWidgets)
library(shiny)
library(dplyr)
library(stringr)
library(purrr)
library(plotly)
library(DT)
library(tidyr)


######################################################
###################### TABLAS UTILES
######################################################
# Cargamos environment con las tablas preprocesadas y con datos agregados
load ('data/tables.RData')

# Per il rango di la data 
df <- data_age_full

# Conversione della colonna 'fecha' in Date
df$fecha <- as.Date(df$fecha)

# 3. Controlli esplorativi e calcolo del range
str(df)
head(df)

# Calcola il range di 'fecha'
date_rng <- range(df$fecha, na.rm = TRUE)
message("Data minima: ", date_rng[1], "; Data massima: ", date_rng[2])


# Set date range fisso
date_range_fixed <- date_rng


# ================================================================================================
#                                  DIZIONARIO DELLE TRADUZIONI
# ================================================================================================



# Per ogni chiave sono fornite 4 versioni: "it" (italiano), "en" (inglese), "es" (spagnolo), "ca" (catalano)
translations <- list(
  titlePanel = list(
    it = "Analisi dei Trend",
    en = "Trend Analysis",
    es = "Análisis de Tendencias",
    ca = "Anàlisi de Tendències"
  ),
  helpTextGeneral = list(
    it = "• Tutti i grafici sono presenti nella sezione Andamenti",
    en = "• All charts are available in the Trend section.",
    es = "• Todos los gráficos están en la sección de Tendencias.",
    ca = "• Tots els gràfics es troben a la secció de Tendències."
  ),
  helpTextGrafInter = list(
    it = "• I grafici sono interattivi, passando il mouse sopra si possono avere informazioni più specifiche.",
    en = "• The charts are interactive; hover the mouse over them to get more detailed information.",
    es = "• Los gráficos son interactivos; pasa el ratón por encima para obtener información más detallada.",
    ca = "• Els gràfics són interactius; passa el ratolí per sobre per obtenir informació més detallada."
  ),
  helpTextGrafInter2 = list(
    it = "I grafici sono interattivi, passando il mouse sopra si possono avere informazioni più specifiche.",
    en = "The charts are interactive; hover the mouse over them to get more detailed information.",
    es = "Los gráficos son interactivos; pasa el ratón por encima para obtener información más detallada.",
    ca = "Els gràfics són interactius; passa el ratolí per sobre per obtenir informació més detallada."
  ),
  helpTextCanc = list(
    it = "• Per cancellare una selezione dal menù a tendina selezionarla e premere canc",
    en = "• To delete a selection from the dropdown menu, select it and press delete",
    es = "• Para eliminar una selección del menú desplegable, selecciónala y presiona suprimir",
    ca = "• Per suprimir una selecció del menú desplegable, selecciona-la i prem suprimir"
  ),
  helpTextZona = list(
    it = "• Zona di Studio include tutte le municipalità",
    en = "• The Study Area includes all municipalities",
    es = "• La Zona de Estudio incluye todos los municipios",
    ca = "• La Zona d'Estudi inclou tots els municipis"
  ),
  helpTextMapInt = list(
    it = "• La mappa è interattiva, cliccando su una zona si possono avere informazioni più specifiche.",
    en = "• The map is interactive; clicking on an area provides more specific information.",
    es = "• El mapa es interactivo; al hacer clic en una zona se obtiene información más específica.",
    ca = "• El mapa és interactiu; en fer clic en una zona s'obté informació més específica."
  ),
  helpTextZona2 = list(
    it = "• Zona di Studio include tutte le municipalità",
    en = "• The Study Area includes all municipalities",
    es = "• La Zona de Estudio incluye todos los municipios",
    ca = "• La Zona d'Estudi inclou tots els municipis"
  ),
  helpTextLento = list(
    it = "• Il grafico potrebbe tardare a caricarsi",
    en = "• The chart may take time to load",
    es = "• El gráfico puede tardar en cargarse",
    ca = "• El gràfic pot trigar a carregar-se"
  ),
  helpTextDesc = list(
    it = "Il grafico fa vedere l'andamento cumulativo dei pazienti, mostrando per esempio il cambio del tipo di paziente da \"cardiovascolare\" a \"entrambi\" quando sorge una diagnosi di \"diabete\". In questo esempio, un paziente prima considerato \"cardiovascolare\" diventerà \"entrambi\", togliendolo dal gruppo \"cardiovascolare\"",
    en = "The chart shows the cumulative trend of patients, illustrating for example the change in patient type from \"cardiovascular\" to \"both\" when a diagnosis of \"diabetes\" occurs. In this case, a patient previously considered \"cardiovascular\" will become \"both\", and will be removed from the \"cardiovascular\" group",
    es = "El gráfico muestra la evolución acumulativa de los pacientes, ilustrando por ejemplo el cambio de tipo de paciente de \"cardiovascular\" a \"ambos\" cuando aparece un diagnóstico de \"diabetes\". En este caso, un paciente previamente considerado \"cardiovascular\" pasará a ser \"ambos\", y será eliminado del grupo \"cardiovascular\"",
    ca = "El gràfic mostra la tendència acumulativa dels pacients, mostrant per exemple el canvi de tipus de pacient de \"cardiovascular\" a \"ambdós\" quan es diagnostica \"diabetis\". En aquest cas, un pacient considerat prèviament \"cardiovascular\" passarà a ser \"ambdós\" i es retirarà del grup \"cardiovascular\""
  ),
  helpTextScelte = list(
    it = "• Si possono scegliere più località, codici postali o loro combinazioni",
    en = "• You can select multiple locations, postal codes, or their combinations",
    es = "• Se pueden seleccionar varias localidades, códigos postales o sus combinaciones",
    ca = "• Es poden seleccionar diverses localitats, codis postals o les seves combinacions"
  ),
  helpTextLegenda = list(
    it = "• Si possono deselezionare etichette direttamente dalla legenda del grafico premendoci sopra",
    en = "• You can deselect labels directly from the chart legend by clicking on them",
    es = "• Se pueden deseleccionar etiquetas directamente desde la leyenda del gráfico haciendo clic en ellas",
    ca = "• Es poden desseleccionar etiquetes directament des de la llegenda del gràfic fent-hi clic"
  ),
  helpTextAct1 = list(
    it = "• I grafici sono interattivi, passando il mouse sopra si possono avere informazioni più specifiche",
    en = "• The charts are interactive; hovering with the mouse reveals more detailed information",
    es = "• Los gráficos son interactivos; al pasar el ratón por encima se obtiene información más específica",
    ca = "• Els gràfics són interactius; si es passa el cursor per sobre es mostren dades més específiques"
  ),
  
  helpTextAct2 = list(
    it = "• C'è la possibilità di scegliere \"Tutto il Periodo\"",
    en = "• There is the option to select \"Entire Period\"",
    es = "• Existe la posibilidad de seleccionar \"Todo el Período\"",
    ca = "• Hi ha la possibilitat de seleccionar \"Tot el Període\""
  ),
  
  helpTextAct3 = list(
    it = "• I grafici mostrano (annualmente o per tutto il periodo di studio) i contatti settimanali con l'intorno sanitario (ambulatorio, ospedale o urgenza) in comparazione con l'andamento della temperatura",
    en = "• The charts show (annually or for the entire study period) weekly contacts with the healthcare surroundings (clinic, hospital, or emergency) compared with the temperature trend",
    es = "• Los gráficos muestran (anualmente o durante todo el período de estudio) los contactos semanales con el entorno sanitario (ambulatorio, hospital o urgencias) en comparación con la evolución de la temperatura",
    ca = "• Els gràfics mostren (anualment o durant tot el període d’estudi) els contactes setmanals amb l’entorn sanitari (ambulatori, hospital o urgències) en comparació amb l’evolució de la temperatura"
  ),
  
  helpTextAct4 = list(
    it = "• Si possono deselezionare etichette direttamente dalla legenda del grafico premendoci sopra",
    en = "• Labels can be deselected directly from the chart legend by clicking on them",
    es = "• Se pueden deseleccionar etiquetas directamente desde la leyenda del gráfico haciendo clic sobre ellas",
    ca = "• Es poden desseleccionar etiquetes directament des de la llegenda del gràfic clicant-hi a sobre"
  ),
  helpTextDiagn = list(
    it = "• Scelta multipla di diagnosi per confrontare",
    en = "• Select multiple diagnoses to compare",
    es = "• Selección múltiple de diagnósticos para comparar",
    ca = "• Selecció múltiple de diagnòstics per comparar"
  ),
  helpTextGen = list(
    it = "• Generale include tutti i pazienti globali dello studio",
    en = "• General includes all global patients in the study",
    es = "• General incluye a todos los pacientes globales del estudio",
    ca = "• General inclou tots els pacients globals de l'estudi"
  ),
  helpTextMapAgg = list(
    it = "• Mappa e grafico si aggiornano in base al periodo scelto e alla selezione delle zone",
    en = "• Map and chart update based on the selected period and zone selection",
    es = "• El mapa y el gráfico se actualizan según el período elegido y la selección de zonas",
    ca = "• El mapa i el gràfic s'actualitzen segons el període seleccionat i la selecció de zones"
  ),
  helpTextNotaLocMapa = list(
    it = "• La legenda si svuota quando si seleziona solo una località (non si possono fare confronti)",
    en = "• The legend clears when only one location is selected (comparisons are not possible)",
    es = "• La leyenda se vacía cuando se selecciona solo una localidad (no se pueden hacer comparaciones)",
    ca = "• La llegenda s'esborra quan se selecciona només una localitat (no es poden fer comparacions)"
  ),
  sliderDateRange = list(
    it = "Seleziona il periodo:",
    en = "Select the period:",
    es = "Seleccione el periodo:",
    ca = "Seleccioneu el període:"
  ),
  all = list(
    it = "Tutto il Periodo",
    en = "Entire Period",
    es = "Todo el período",
    ca = "Tot el període"
  ),
  selectDiagnosis = list(
    it = "Seleziona le diagnosi:",
    en = "Select the diagnoses:",
    es = "Seleccione los diagnósticos:",
    ca = "Seleccioneu els diagnòstics:"
  ),
  trendTempTitle = list(
    it = "Trend della Temperatura",
    en = "Temperature Trend",
    es = "Tendencia de la Temperatura",
    ca = "Tendència de la Temperatura"
  ),
  trendTempand = list(
    it = "Temperatura",
    en = "Temperature",
    es = "Temperatura",
    ca = "Temperatura"
  ),
  trendDiagnTitle = list(
    it = "Trend delle Diagnosi",
    en = "Diagnosis Trend",
    es = "Tendencia de Diagnósticos",
    ca = "Tendència dels Diagnòstics"
  ),
  etaMediaTitle = list(
    it = "Distribuzione Generale dell'Età con Media",
    en = "Overall Age Distribution with Mean",
    es = "Distribución General de Edad con Media",
    ca = "Distribució General de l'Edat amb Mitjana"
  ),
  otherGrafico2Title = list(
    it = "Attività settimanale per gruppo", 
    en = "Weekly group activity", 
    es = "Actividad semanal por grupo", 
    ca = "Activitat setmanal per grup"
  ),
  xAge = list(
    it = "Età",
    en = "Age",
    es = "Edad",
    ca = "Edat"
  ),
  Agemed = list(
    it = "Età Media",
    en = "Mean Age",
    es = "Edad Media",
    ca = "Edat Mitjana"
  ),
  yFrequency = list(
    it = "Frequenza",
    en = "Frequency",
    es = "Frecuencia",
    ca = "Freqüència"
  ),
  countss = list(
    it = "Frequenza",
    en = "Frequency",
    es = "Frecuencia",
    ca = "Freqüència"
  ),
  distEtaDiagTitle = list(
    it = "Distribuzione Età",
    en = "Age Distribution",
    es = "Distribución de Edad",
    ca = "Distribució de l'Edat"
  ),
  distSessoTitle = list(
    it = "Distribuzione per Sesso",
    en = "Sex Distribution",
    es = "Distribución por Sexo",
    ca = "Distribució per Sexe"
  ),
  distSessoDiagTitle = list(
    it = "Distribuzione per Sesso per Diagnostico",
    en = "Sex Distribution by Diagnosis",
    es = "Distribución por Sexo por Diagnóstico",
    ca = "Distribució per Sexe per Diagnòstic"
  ),
  trendSessoDiagnTitle = list(
    it = "Andamento Temporale: Distribuzione del Sesso",
    en = "Temporal Trend: Sex Distribution",
    es = "Tendencia Temporal: Distribución por Sexo",
    ca = "Tendència Temporal: Distribució del Sexe"
  ),
  postalGenTitle = list(
    it = "Distribuzione dei Codici Postali (Generale)",
    en = "Postal Codes Distribution (General)",
    es = "Distribución de los Códigos Postales (General)",
    ca = "Distribució dels Codis Postals (General)"
  ),
  postalCardioTitle = list(
    it = "Codici Postali - Cardiovascolare",
    en = "Postal Codes - Cardiovascular",
    es = "Códigos Postales - Cardiovascular",
    ca = "Codis Postals - Cardiovascular"
  ),
  postalDiabTitle = list(
    it = "Codici Postali - Diabete",
    en = "Postal Codes - Diabetes",
    es = "Códigos Postales - Diabetes",
    ca = "Codis Postals - Diabetes"
  ),
  postalAmbosTitle = list(
    it = "Codici Postali - Entrambi",
    en = "Postal Codes - Both",
    es = "Códigos Postales - Ambos",
    ca = "Codis Postals - Ambos"
  ),
  postalNote1 = list(
    it = "Nota: I codici postali con conteggio nullo sono stati esclusi.",
    en = "Note: Postal codes with zero count have been excluded.",
    es = "Nota: Los códigos postales con recuento cero han sido excluidos.",
    ca = "Nota: Els codis postals amb recompte zero han estat exclosos."
  ),
  postalNote2 = list(
    it = "Nota: I codici postali con conteggio nullo sono stati esclusi.",
    en = "Note: Postal codes with zero count have been excluded.",
    es = "Nota: Los códigos postales con recuento cero han sido excluidos.",
    ca = "Nota: Els codis postals amb recompte zero han estat exclosos."
  ),
  postalNote3 = list(
    it = "Nota: I codici postali con conteggio nullo sono stati esclusi.",
    en = "Note: Postal codes with zero count have been excluded.",
    es = "Nota: Los códigos postales con recuento cero han sido excluidos.",
    ca = "Nota: Els codis postals amb recompte zero han estat exclosos."
  ),
  postalNote4 = list(
    it = "Nota: I codici postali con conteggio nullo sono stati esclusi.",
    en = "Note: Postal codes with zero count have been excluded.",
    es = "Nota: Los códigos postales con recuento cero han sido excluidos.",
    ca = "Nota: Els codis postals amb recompte zero han estat exclosos."
  ),
  date = list(
    it = "Data",
    en = "Date",
    es = "Fecha",
    ca = "Data"
  ),
  tit3 = list(
    it = "Data (settimane)",
    en = "Date (weeks)",
    es = "Fecha (semanas)",
    ca = "Data (setmanes)"
  ),
  avg_temp = list(
    it = "Temperatura Media °C",
    en = "Average Temperature °C",
    es = "Temperatura Media °C",
    ca = "Temperatura Mitjana °C"
  ),
  max_temp=list(
    it = "Temperatura Massima °C",
    en = "Maximum Temperature °C",
    es = "Temperatura Máxima °C",
    ca = "Temperatura Màxima °C"
  ),
  postalTabTitle = list(
    it = "Tab Codice Postale",
    en = "Postal Code Tab",
    es = "Pestaña Código Postal",
    ca = "Pestanya Codi Postal"
  ),
  selectPostalPlotType = list(
    it = "Scegli tipo di grafico",
    en = "Select plot type",
    es = "Seleccione tipo de gráfico",
    ca = "Seleccioneu tipus de gràfic"
  ),
  postal_plot_standard = list(
    it = "Pazienti per Codice Postale",
    en = "Patients by Postal Code",
    es = "Pacientes por Código Postal",
    ca = "Pacients per Codi Postal"
  ),
  postal_plot_mirror = list(
    it = "Pazienti vs Popolazione",
    en = "Patients vs Population",
    es = "Pacientes vs Población",
    ca = "Pacients vs Població"
  ),
  postal_code = list(
    it = "Codice Postale",
    en = "Postal Code",
    es = "Código Postal",
    ca = "Codi Postal"
  ),
  patients = list(
    it = "Pazienti",
    en = "Patients",
    es = "Pacientes",
    ca = "Pacients"
  ),
  non_patients = list(
    it = "Popolazione non‐Pazienti",
    en = "Non‐Patients Population",
    es = "Población no‐Pacientes",
    ca = "Població no‐Pacients"
  ),
  y_axis_title = list(
    it = "Località",
    en = "Locality",
    es = "Localidad",
    ca = "Localitat"
  ),
  percent_patients = list(
    it = "% Pazienti",
    en = "% Patients",
    es = "% Pacientes",
    ca = "% Pacients"
  ),
  percent_non_patients = list(
    it = "% Pop. non‐Pazienti",
    en = "% Non‐Patients",
    es = "% No‐Pacientes",
    ca = "% No‐Pacients"
  ),
  population_total = list(
    it = "Abitanti Tot.",
    en = "Total Pop.",
    es = "Habitantes Tot.",
    ca = "Habitants Tot."
  ),
  n_patients = list(
    it = "# Pazienti",
    en = "# Patients",
    es = "# Pacientes",
    ca = "# Pacients"
  ),
  percent_column = list(
    it = "% Pazienti",
    en = "% Patients",
    es = "% Pacientes",
    ca = "% Pacients"
  ),
  locality = list(
    it = "Località",
    en = "Locality",
    es = "Localidad",
    ca = "Localitat"
  ),
  patient_distribution_postal_code = list(
    it = "Distribuzione Pazienti per codice postale",
    en = "Patient Distribution by Postal Code",
    es = "Distribución de Pacientes por Código Postal",
    ca = "Distribució de Pacients per Codi Postal"
  ),
  diagnosis_count = list(
    it = "Numero di Diagnosi",
    en = "Number of Diagnoses",
    es = "Número de Diagnósticos",
    ca = "Nombre de Diagnòstics"
  ),
  year_month = list(
    it = "Anni",
    en = "Years",
    es = "Años",
    ca = "Anys"
  ),
  event_count = list(
    it = "Numero di Eventi",
    en = "Number of Events",
    es = "Número de Eventos",
    ca = "Nombre d'Esdeveniments"
  ),
  patient_count = list(
    it = "Numero di Pazienti",
    en = "Number of Patients",
    es = "Número de Pacientes",
    ca = "Nombre de Pacients"
  ),
  eventCount = list(
    it = "Numero di Pazienti",
    en = "Number of Patients",
    es = "Número de Pacientes",
    ca = "Nombre de Pacients"
  ),
  sesso23=list(
    it = "Sesso",
    en = "Sex",
    es = "Sexo",
    ca = "Sexe"
  ),
  selectAllLabel = list(
    it = "Seleziona/Deseleziona Tutto",
    en = "Select/Deselect All",
    es = "Seleccionar/Deseleccionar Todo",
    ca = "Selecciona/Desselecciona Tot"
  ),
  selectDiagnosis = list(
    it = "Seleziona le diagnosi:",
    en = "Select the diagnoses:",
    es = "Seleccione los diagnósticos:",
    ca = "Seleccioneu els diagnòstics:"
  ),
  tipoPaziente = list(
    it = "Tipo Paziente",
    en = "Patient Type",
    es = "Tipo de Paciente",
    ca = "Tipus de Pacient"
  ),
  tipoPaziente2 = list(
    it = "Tipo Paziente",
    en = "Patient Type",
    es = "Tipo de Paciente",
    ca = "Tipus de Pacient"
  ),
  Control = list(
    it = "Controllo",
    en = "Control",
    es = "Control",
    ca = "Control"
  ),
  Cardiovascular = list(
    it = "Cardiovascolare",
    en = "Cardiovascular",
    es = "Cardiovascular",
    ca = "Cardiovascular"
  ),
  Diabetes = list(
    it = "Diabete",
    en = "Diabetes",
    es = "Diabetes",
    ca = "Diabetes"  # oppure "Diabetis" se preferisci
  ),
  Ambos = list(
    it = "Entrambi",
    en = "Both",
    es = "Ambos",
    ca = "Ambdós"
  ),
  map_popup_info=list(
    it = "Diagnosi in questa area:",
    en = "Diagnoses in this area:",
    es = "Diagnósticos en esta área:",
    ca = "Diagnòstics en aquesta àrea:"
  ),
  
  map_legend_title=list(
    it = "Intensità Diagnosi",
    en = "Diagnosis Intensity",
    es = "Intensidad de Diagnóstico",
    ca = "Intensitat de Diagnòstic"
  ),
  periodoOsservazioneTitle = list(
    it = "Periodo di osservazione",
    en = "Observation period",
    es = "Período de observación",
    ca = "Període d'observació"
  ),
  periodoOsservazioneTemp = list(
    it = "Periodo di osservazione",
    en = "Observation period",
    es = "Período de observación",
    ca = "Període d'observació"
  ),
  periodoOsservazioneSex = list(
    it = "Periodo di osservazione",
    en = "Observation period",
    es = "Período de observación",
    ca = "Període d'observació"
  ),
  
  selezionaLocalitaTitle = list(
    it = "Seleziona una località:",
    en = "Select a location:",
    es = "Selecciona una localidad:",
    ca = "Selecciona una localitat:"
  ),
  
  zonaStudioTitle = list(
    it = "Zona di studio",
    en = "Study area",
    es = "Área de estudio",
    ca = "Àrea d'estudi"
  ),
  Generale = list(
    it = "Generale",
    en = "General",
    es = "General",
    ca = "General"
  ),
  selectSessoModeLabel = list(
    it = "Seleziona modalità di visualizzazione:",
    en = "Select display mode:",
    es = "Seleccione modo de visualización:",
    ca = "Seleccioneu el mode de visualització:"
  ),
  trendTipTitle = list(
    it = "Andamenti",
    en = "Trends",
    es = "Tendencias",
    ca = "Tendències"
  ),
  mapGenTitle = list(
    it = "Mappa",
    en = "Map",
    es = "Mapa",
    ca = "Mapa"
  ),
  municipality = list(
    it = "Municipio",
    en = "Municipality",
    es = "Municipio",
    ca = "Municipi"
  ),
  summaryTitle = list(
    it="Sintesi pazienti",
    en= "Patient summary",
    es="Resumen de pacientes",
    ca="Resum de pacients"
  ),
  totalPatients = list(
    it = "Totale pazienti",
    en = "Total patients",
    es = "Total de pacientes",
    ca = "Total de pacients"
  ),
  
  percentage = list(
    it = "Percentuale",
    en = "Percentage",
    es = "Porcentaje",
    ca = "Percentatge"
  ),
  SelectSpecCod = list(
    it = "Seleziona un Codice Postale Specifico:",
    en = "Select a Specific Codpost:",
    es = "Selecciona un Código Postal Específico:",
    ca = "Selecciona un Codi Postal Específic:"
  ),
  # Tab “Diagnosi”
  diagnTabTitle = list(
    it = "Diagnosi",
    en = "Diagnoses",
    es = "Diagnósticos",
    ca = "Diagnòstics"
  ),
  
  # Titolo del grafico cumulativo
  cumTrendTitle = list(
    it = "Trend giornaliero cumulativo",
    en = "Cumulative daily trend",
    es = "Tendencia diaria acumulada",
    ca = "Tendència diària acumulada"
  ),
  
  # Sottotitoli dei pie-charts
  cardioPieTitle = list(
    it = "Proporzione - Cardiovascolare",
    en = "Breakdown – Cardiovascular",
    es = "Proporción – Cardiovascular",
    ca = "Proporció – Cardiovascular"
  ),
  diabPieTitle = list(
    it = "Proporzione – Diabete",
    en = "Breakdown – Diabetes",
    es = "Proporción – Diabetes",
    ca = "Proporció – Diabetes"
  ),
  selectView = list(
    it = "Visualizza:",
    en = "Display:",
    es = "Mostrar:",
    ca = "Mostra:"
  ),
  ambosPieTitle = list(
    it = "Combinazioni – Entrambi",
    en = "Combinations – Both",
    es = "Combinaciones – Ambos",
    ca = "Combinacions – Ambdós"
  ),
  
  # Label per i codici ICD-10
  icd10 = list(
    it = "Codice ICD-10",
    en = "ICD-10 code",
    es = "Código ICD-10",
    ca = "Codi ICD-10"
  ),
  
  # Label per la legenda delle combinazioni
  combination = list(
    it = "Combinazione",
    en = "Combination",
    es = "Combinación",
    ca = "Combinació"
  ),
  selectDiagnView = list(
    it = "Visualizza:",
    en = "Display:",
    es = "Mostrar:",
    ca = "Mostra:"
  ),
  viewTrend = list(
    it = "Andamento tipologia paziente",
    en = "Patient-type trend",
    es = "Tendencia tipo paciente",
    ca = "Tendència tipus pacient"
  ),
  viewDist = list(
    it = "Distribuzione diagnostico",
    en = "Diagnostic distribution",
    es = "Distribución diagnóstica",
    ca = "Distribució diagnòstica"
  ),
  weeklyActTitle=list(
    it = "Attività settimanale + temperatura",
    en = "Weekly Activity + Temperature",
    es = "Actividad semanal + Temperatura",
    ca = "Activitat setmanal + Temperatura"
  ),
  activityChartsTitle = list(
    it = "Grafici dell’attività",
    en = "Activity Charts",
    es = "Gráficos de actividad",
    ca = "Gràfics d’activitat"
  ),
  selectDateRange=list(
    it = "Seleziona periodo:",
    en = "Select date range:",
    es = "Seleccione período:",
    ca = "Seleccione període:"
  ),
  selectYear=list(
    it = "Anno",
    en = "Year",
    es = "Año",
    ca = "Any"
  ),
  otherCategory=list(
    it="Altri",
    en="Other",
    es="Otros",
    ca="Altres"
  ),
  week = list(
    it = "Settimana",
    en = "Week",
    es = "Semana",
    ca = "Setmana"
  ),
  trendTempTitle1 = list(
    it = "Temperatura",
    en = "Temperature",
    es = "Temperatura",
    ca = "Temperatura"
  ),
  normalized_scale = list(
    it = "Scala normalizzata",
    en = "Normalized Scale",
    es = "Escala normalizada",
    ca = "Escala normalitzada"
  ),
  trendDiagnTitle1 = list(
    it = "Diagnosi",
    en = "Diagnoses",
    es = "Diagnósticos",
    ca = "Diagnòstics"
  ),
  
  distSessoTitle1 = list(
    it = "Sesso",
    en = "Sex",
    es = "Sexo",
    ca = "Sexe"
  ),
  
  trendSessoDiagnTitle1 = list(
    it = "Andamento",
    en = "Trend",
    es = "Tendencia",
    ca = "Tendència"
  ),
  
  etaMediaTitle1 = list(
    it = "Età",
    en = "Age",
    es = "Edad",
    ca = "Edat"
  ),
  
  postalGenTitle1 = list(
    it = "CP",
    en = "PC",
    es = "CP",
    ca = "CP"
  ),
  
  activityChartsTitle1 = list(
    it = "Attività",
    en = "Activity",
    es = "Actividad",
    ca = "Activitat"
  ),
  group = list(
    it = "Gruppo",
    en = "Group",
    es = "Grupo",
    ca = "Grup"
  ),
  count = list(
    it = "Conteggio",
    en = "Count",
    es = "Conteo",
    ca = "Compte"
  ),
  # facoltativo: etichetta per la linea di temperatura
  trendTempand = list(
    it = "Andamento della temperatura",
    en = "Temperature trend",
    es = "Tendencia de la temperatura",
    ca = "Tendència de la temperatura"
  ),
  selectDatesManually  = list(it = "Oppure seleziona manualmente:", en = "Or select dates manually:", es = "O selecciona manualmente:", ca = "O selecciona manualment:"),
  selectLocation = list(
    it = "Seleziona Località:",
    en = "Select Location:",
    es = "Selecciona Localidad:",
    ca = "Selecciona Localitat:"
  ),
  selectLocation22 = list(
    it = "Località:",
    en = "Location:",
    es = "Localidad:",
    ca = "Localitat:"
  ),
  helpTextICD10 <- list(
    es = "
    <b>CÓDIGOS CIE-10 MÁS COMUNES ENTRE LOS PACIENTES</b>
    <b>Diccionario de Códigos ICD10 (Cardiovascular):</b>
    
    • <b>I10:</b>  Hipertensión  (acelerada, arterial, benigna, esencial...)  
    • <b>I35:</b> Trastornos de válvula aórtica no reumáticos  
    • <b>I48:</b> Fibrilación y flutter auricular  
        <b>I48.9:</b> Fibrilación y flutter auricular no especificados  
          <b>I48.91:</b> Fibrilación auricular no especificada  
    • <b>I50:</b> Insuficiencia cardiaca  
        <b>I50.9:</b> Insuficiencia cardiaca, no especificada  
    • <b>I69:</b> Secuelas de enfermedad cerebrovascular  
        <b>I69.9:</b> Secuelas de enfermedades cerebrovasculares no especificadas  
          <b>I69.99:</b> Otras secuelas de enfermedad cerebrovascular no especificada  
             <b>I69.998:</b> Otras secuelas tras enfermedad cerebrovascular no especificada  
    • <b>I79:</b> Trastornos de arterias, arteriolas y capilares en enfermedades clasificadas bajo otro concepto  
        <b>I79.8:</b> Otros trastornos de arterias, arteriolas y capilares en enfermedades clasificadas bajo otro concepto  
    • <b>I83:</b> Venas varicosas de extremidades inferiores  
        <b>I83.9:</b> Venas varicosas asintomáticas de extremidades inferiores  
          <b>I83.90:</b> Venas varicosas asintomáticas de extremidad inferior no especificada
    ",
    
    it = "
    <b>CODICI ICD-10 PIÙ COMUNI TRA I PAZIENTI</b>
    <b>Dizionario Codici ICD10 (Cardiovascolare):</b>
    
    • <b>I10:</b> Ipertensione (accelerata, arteriosa, benigna, essenziale...)  
    • <b>I35:</b> Disturbi non reumatici della valvola aortica  
    • <b>I48:</b> Fibrillazione e flutter atriale  
        <b>I48.9:</b> Fibrillazione e flutter atriale non specificati  
          <b>I48.91:</b> Fibrillazione atriale non specificata  
    • <b>I50:</b> Insufficienza cardiaca  
        <b>I50.9:</b> Insufficienza cardiaca non specificata  
    • <b>I69:</b> Sequele di malattia cerebrovascolare  
        <b>I69.9:</b> Sequele di malattie cerebrovascolari non specificate  
          <b>I69.99:</b> Altre sequele di malattia cerebrovascolare non specificata  
            <b>I69.998:</b> Altre sequele successive a malattia cerebrovascolare non specificata  
    • <b>I79:</b> Disturbi di arterie, arterioli e capillari in  malattie classificate sotto altro concetto  
        <b>I79.8:</b> Altri disturbi di arterie, arterioli e capillari in malattie classificate sotto altro concetto  
    • <b>I83:</b> Vene varicose degli arti inferiori  
        <b>I83.9:</b> Vene varicose asintomatiche degli arti inferiori  
          <b>I83.90:</b> Vene varicose asintomatiche di arto inferiore non specificata
    ",
    
    en = "
    <b>MOST COMMON ICD-10 CODES AMONG PATIENTS</b>
    <b>ICD10 Code Dictionary (Cardiovascular):</b> 
    
    • <b>I10:</b> Hypertension (accelerated, arterial, benign, essential...)  
    • <b>I35:</b> Nonrheumatic aortic valve disorders  
    • <b>I48:</b> Atrial fibrillation and flutter  
        <b>I48.9:</b> Unspecified atrial fibrillation and flutter  
          <b>I48.91:</b> Unspecified atrial fibrillation  
    • <b>I50:</b> Heart failure  
        <b>I50.9:</b> Heart failure, unspecified  
    • <b>I69:</b> Sequelae of cerebrovascular disease  
        <b>I69.9:</b> Unspecified sequelae of cerebrovascular diseases  
          <b>I69.99:</b> Other sequelae of unspecified cerebrovascular disease  
             <b>I69.998:</b> Other sequelae following unspecified cerebrovascular disease  
    • <b>I79:</b> Disorders of arteries, arterioles and capillaries in diseases classified elsewhere  
        <b>I79.8:</b> Other disorders of arteries, arterioles and capillaries in diseases classified elsewhere  
    • <b>I83:</b> Varicose veins of lower extremities  
        <b>I83.9:</b> Asymptomatic varicose veins of lower extremities  
          <b>I83.90:</b> Unspecified asymptomatic varicose vein of lower extremity
    ",
    
    ca = "
    <b>CODIS CIE-10 MÉS COMUNS ENTRE ELS PACIENTS</b>
    <b>Diccionari de Codis ICD10 (Cardiovascular):</b>
    
    • <b>I10:</b> Hipertensió (accelerada, arterial, benigna, essencial...)  
    • <b>I35:</b> Trastorns no reumàtics de la vàlvula aòrtica  
    • <b>I48:</b> Fibril·lació i flutter auricular  
        <b>I48.9:</b> Fibril·lació i flutter auricular no especificats  
          <b>I48.91:</b> Fibril·lació auricular no especificada  
    • <b>I50:</b> Insuficiència cardíaca  
        <b>I50.9:</b> Insuficiència cardíaca no especificada  
    • <b>I69:</b> Seqüeles de malaltia cerebrovascular  
        <b>I69.9:</b> Seqüeles de malalties cerebrovasculars no especificades  
          <b>I69.99:</b> Altres seqüeles de malaltia cerebrovascular no especificada  
             <b>I69.998:</b> Altres seqüeles posteriors a malaltia cerebrovascular no especificada  
    • <b>I79:</b> Trastorns d’artèries, arterioles i capil·lars en malalties classificades sota un altre concepte  
        <b>I79.8:</b> Altres trastorns d’artèries, arterioles i capil·lars en malalties classificades sota un altre concepte  
    • <b>I83:</b> Vena varicoses de les extremitats inferiors  
        <b>I83.9:</b> Vena varicosa asintomàtica de les extremitats inferiors  
          <b>I83.90:</b> Vena varicosa asintomàtica de l’extremitat inferior no especificada
    "
  ),
  helpTextICD11 <- list(
    es = "
    <b>CÓDIGOS CIE-10 MÁS COMUNES ENTRE LOS PACIENTES</b>
    <b>Diccionario de Códigos ICD10 (Diabetes):</b>
    
    • <b>E11:</b> Diabetes mellitus tipo 2  
        <b>E11.9:</b> Diabetes mellitus tipo 2 sin complicaciones  
        <b>E11.6:</b> Diabetes mellitus tipo 2 con otras complicaciones especificadas
    ",
    
    it = "
    <b>CODICI ICD-10 PIÙ COMUNI TRA I PAZIENTI</b>
    <b>Dizionario Codici ICD10 (Diabete):</b>
    
    • <b>E11:</b> Diabete mellito di tipo 2  
        <b>E11.9:</b> Diabete mellito di tipo 2 senza complicazioni  
        <b>E11.6:</b> Diabete mellito di tipo 2 con altre complicazioni specificate
    ",
    
    en = "
    <b>MOST COMMON ICD-10 CODES AMONG PATIENTS</b>
    <b>ICD10 Code Dictionary (Diabetes):</b> 
    
    • <b>E11:</b> Type 2 diabetes mellitus  
        <b>E11.9:</b> Type 2 diabetes mellitus without complications  
        <b>E11.6:</b> Type 2 diabetes mellitus with other specified complications
    ",
    
    ca = "
    <b>CODIS CIE-10 MÉS COMUNS ENTRE ELS PACIENTS</b>
    <b>Diccionari de Codis ICD10 (Diabetis):</b>
    
    • <b>E11:</b> Diabetis mellitus tipus 2  
        <b>E11.9:</b> Diabetis mellitus tipus 2 sense complicacions  
        <b>E11.6:</b> Diabetis mellitus tipus 2 amb altres complicacions especificades
    "
  ),
  helpTextICDamb <- list(
    es = "
    <b>CÓDIGOS CIE-10 MÁS COMUNES ENTRE LOS PACIENTES</b>
    <b>Diccionario de Códigos ICD10:</b>
    
    • <b>I10+E11.9:</b>
         <b>I10:</b> Hipertensió (accelerada, arterial, benigna, essencial...)  
         <b>E11.9:</b> Diabetes mellitus tipo 2 sin complicaciones  
    • <b>I50.9+E11.9:</b>
         <b>I50.9:</b> Insuficiencia cardiaca, no especificada 
         <b>E11.9:</b> Diabetes mellitus tipo 2 sin complicaciones  
    • <b>I48.91+E11.9:</b>
         <b>I48.91:</b> Fibrilación auricular no especificada  
         <b>E11.9:</b> Diabetes mellitus tipo 2 sin complicaciones  
    • <b>I79.8+E11.9:</b>
         <b>I79.8:</b> Otros trastornos de arterias, arteriolas y capilares en enfermedades clasificadas bajo otro concepto 
         <b>E11.9:</b> Diabetes mellitus tipo 2 sin complicaciones  
    ",
    
    it = "
    <b>CODICI ICD-10 PIÙ COMUNI TRA I PAZIENTI</b>
    <b>Dizionario Codici ICD10:</b>
    
    • <b>I10+E11.9:</b>
         <b>I10:</b> Ipertensione (accelerata, arteriosa, benigna, essenziale...)  
         <b>E11.9:</b> Diabete mellito di tipo 2 senza complicazioni  
    • <b>I50.9+E11.9:</b>
         <b>I50.9:</b> Insufficienza cardiaca non specificata  
         <b>E11.9:</b> Diabete mellito di tipo 2 senza complicazioni  
    • <b>I48.91+E11.9:</b>
         <b>I48.91:</b> Fibrillazione atriale non specificata  
         <b>E11.9:</b> Diabete mellito di tipo 2 senza complicazioni  
    • <b>I79.8+E11.9:</b>
         <b>I79.8:</b> Altri disturbi di arterie, arterioli e capillari in malattie classificate sotto altro concetto
         <b>E11.9:</b> Diabete mellito di tipo 2 senza complicazioni  
    ",
    ca = "
    <b>CODIS CIE-10 MÉS COMUNS ENTRE ELS PACIENTS</b>
    <b>Diccionari de Codis ICD10:</b>
    
    • <b>I10+E11.9:</b>
         <b>I10:</b> Hipertensió (accelerada, arterial, benigna, essencial...)  
         <b>E11.9:</b> Diabetis mellitus tipus 2 sense complicacions  
    • <b>I50.9+E11.9:</b>
         <b>I50.9:</b> Insuficiència cardíaca no especificada  
         <b>E11.9:</b> Diabetis mellitus tipus 2 sense complicacions  
    • <b>I48.91+E11.9:</b>
         <b>I48.91:</b> Fibril·lació auricular no especificada  
         <b>E11.9:</b> Diabetis mellitus tipus 2 sense complicacions  
    • <b>I79.8+E11.9:</b>
         <b>I79.8:</b> Altres trastorns d’artèries, arterioles i capil·lars en malalties classificades sota un altre concepte 
         <b>E11.9:</b> Diabetis mellitus tipus 2 sense complicacions  
    ",
    en = "
    <b>MOST COMMON ICD-10 CODES AMONG PATIENTS</b>
    <b>ICD10 Code Dictionary:</b> 
    
    • <b>I10+E11.9:</b>
         <b>I10:</b> Hypertension (accelerated, arterial, benign, essential...)  
         <b>E11.9:</b> Type 2 diabetes mellitus without complications  
    • <b>I50.9+E11.9:</b>
         <b>I50.9:</b> Heart failure, unspecified  
         <b>E11.9:</b> Type 2 diabetes mellitus without complications  
    • <b>I48.91+E11.9:</b>
         <b>I48.91:</b> Unspecified atrial fibrillation  
         <b>E11.9:</b> Type 2 diabetes mellitus without complications  
    • <b>I79.8+E11.9:</b>
         <b>I79.8:</b> Other disorders of arteries, arterioles and capillaries in diseases classified elsewhere
         <b>E11.9:</b> Type 2 diabetes mellitus without complications  
    ")
  
  
  # Aggiungi altre chiavi se necessario...
)

gender_labels <- list(
  M = list(     # Valore originale per "mujer"
    it = "D",   # Donna
    en = "W",   # Woman
    es = "M",  # Puoi lasciare "Mujer" oppure tradurre come preferisci
    ca = "D"    # Dona
  ),
  V = list(     # Valore originale per "varón"
    it = "U",   # Uomo
    en = "M",   # Man
    es = "H",  # Oppure "M" se preferisci
    ca = "H"    # Home
  )
)

# Dizionario che associa le località ai codici postali
localita_codici_postali <- list(
  "Terrassa" = c("8221", "8225", "8222", "8226", "8223", "8227", "8224", "8228"),
  "Sant Cugat del Vallés" = c("8172", "8173", "8174", "8195", "8196", "8198", "8197"),
  "Rubí" = c("8191"),
  "Sant Quirze del Vallés" = c("8192", "8194"),
  "Ullastrell" = c("8231"),
  "Viladecavalls" = c("8232"),
  "Vacarisses" = c("8233"),
  "Matadepera" = c("8230")
)
mean_age_translations <- list(
  label = list(
    it = "Età media",
    en = "Mean age",
    es = "Edad media",
    ca = "Edat mitjana"
  ),
  suffix = list(
    it = "anni",
    en = "years",
    es = "años",
    ca = "anys"
  )
)

tipo_PI_translations <- list(
  Control        = list(it = "Controllo",        en = "Control",        es = "Control",        ca = "Control"),
  Cardiovascular = list(it = "Cardiovascolare",   en = "Cardiovascular", es = "Cardiovascular", ca = "Cardiovascular"),
  Diabetes       = list(it = "Diabete",           en = "Diabetes",       es = "Diabetes",       ca = "Diabetes"),
  Ambos          = list(it = "Entrambi",          en = "Both",           es = "Ambos",          ca = "Ambdós")
)


# ==========================================================================================
#                                FUNCIONES PARA OBTENER TRADUCCION
# ==========================================================================================


t <- function(key, lang) {
  if(!is.null(translations[[key]])) {
    return(translations[[key]][[lang]])
  } else {
    return(key)
  }
}


translate_tipo_P <- function(tipos, lang) {
  sapply(tipos, function(x) {
    if (!is.null(translations[[x]]) && !is.null(translations[[x]][[lang]])) {
      translations[[x]][[lang]]
    } else {
      x
    }
  }, USE.NAMES = FALSE)
}

translate_gender <- function(gender, lang) {
  if (!is.null(gender_labels[[gender]]) && !is.null(gender_labels[[gender]][[lang]])) {
    return(gender_labels[[gender]][[lang]])
  } else {
    return(gender)
  }
}

t_mean_age <- function(part, lang) {
  # part deve essere "label" oppure "suffix"
  mean_age_translations[[part]][[lang]]
}

t_tipo_P <- function(key, lang) {
  if (!is.null(tipo_PI_translations[[key]])) {
    return(tipo_PI_translations[[key]][[lang]])
  } else {
    return(key)
  }
}

translate_tipo_P_new <- function(vettore, lang) {
  sapply(vettore, function(x) t_tipo_P(x, lang), USE.NAMES = FALSE)
}


#################################################### POSTAL CODE CODES ###########
##################################################################################
choices_filtrate <- choices_sex_diag[choices_sex_diag != "Ambos"]



###############################################################################
##################### CREAZIONE DATAFRAME POSTAL PLOT #####################################

# 1) **Unica volta** definisco il dizionario località → codici postali
localita_codici_postali2 <- list(
  "Terrassa"               = c("8221","8225","8222","8226","8223","8227","8224","8228"),
  "Sant Cugat del Vallés"  = c("8172","8173","8174","8195","8196","8198","8197"),
  "Rubí"                   = c("8191"),
  "Sant Quirze del Vallés" = c("8192","8194"),
  "Ullastrell"             = c("8231"),
  "Viladecavalls"          = c("8232"),
  "Vacarisses"             = c("8233"),
  "Matadepera"             = c("8230")
)

# 2) Se voglio davvero due copie indipendenti (per evitare sovrapposizioni future),
#    trasformo ogni tibble in un as_tibble separato:
library(purrr)
postal_standard <- map(postal_summary_list, ~ as_tibble(.x))
postal_mirror   <- map(postal_summary_list, ~ as_tibble(.x))

# 3) Popolazione per località (usato solo dal mirror plot)
pop_by_loc <- tibble::tribble(
  ~locality,               ~total_pop,
  "Terrassa",              224114,
  "Sant Cugat del Vallés",  95725,
  "Rubí",                   79007,
  "Sant Quirze del Vallés", 20100,
  "Ullastrell",             2139,
  "Viladecavalls",         7644,
  "Vacarisses",            7325,
  "Matadepera",            9752
)

# 4) Filtro codici postali (oppure NULL se non filtrare)
codici_utili2 <- NULL

# 5) Preparo **una sola volta** il lookup codpost ↔ località
codpost_localita_df <- do.call(
  rbind,
  lapply(names(localita_codici_postali2), function(loc) {
    data.frame(
      codpost  = stringr::str_pad(localita_codici_postali2[[loc]], width = 5, side = "left", pad = "0"),
      localita = loc,
      stringsAsFactors = FALSE
    )
  })
)
# =======================
# UI: INTERFACCIA UTENTE
# =======================
ui <- fluidPage(
  useShinyjs(),
  
  tags$head(
    includeCSS("CSS/styles.css"),
    tags$style(HTML("
      body, html {
        height: 100%;
        margin: 0;
        overflow: hidden;
      }
      #map {
        position: absolute;
        top: 0; left: 0;
        bottom: 0; right: 60px;
        z-index: 1;
      }
      #sidebar {
        position: absolute;
        top: 0;
        right: 0;
        width: 50%;
        height: 100%;
        background-color: white;
        border-left: 1px solid #ccc;
        box-shadow: -2px 0 5px rgba(0,0,0,0.1);
        overflow-y: auto;
        display: none;
        z-index: 2;
        transition: top 0.3s ease-in-out;
        padding-left: 30px;
        padding-right: 65px;
      }
      .leaflet-control.custom-legend {
  width: 75px !important;
  font-size: 9px !important;
  padding: 4px !important;
  background-color: rgba(255,255,255,0.9) !important;
  box-shadow: 0 0 5px rgba(0,0,0,0.3);
  border-radius: 5px;
}
.leaflet-control.custom-legend i {
  margin-right: -1px !important; /* riduce lo spazio tra il colore e il testo */
  width: 3px !important;       /* puoi anche ridurre la larghezza del box colore */
  height: 8px !important;
}
.grayed-out {
  opacity: 0.5;
  pointer-events: none;
}


.custom-slider-sex .irs--flat .irs-bar {
  background-color: #00BFC4 !important;
}
.custom-slider-sex .irs--flat .irs-line {
  background-color: #afdadb !important;
}
.custom-slider-sex .irs--flat .irs-from {
  background-color: #00BFC4 !important;
  border: 2px solid #00BFC4 !important;
  color: #fff;
}
.custom-slider-sex .irs--flat .irs-from:before,
.custom-slider-sex .irs--flat .irs-to:before,
.custom-slider-sex .irs--flat .irs-single:before {
  border-top-color: #00BFC4 !important;  /* colore nuovo */
}

.custom-slider-sex .irs--flat .irs-to {
  background-color: #00BFC4 !important;
  border: 2px solid #00BFC4 !important;
  color: #fff;
}
.custom-slider-sex .irs--flat .irs-handle>i:first-child
 {
  background-color: #fff !important;
  border: 2px solid #00BFC4 !important;
}

      .vertical-tabs {
        position: absolute;
        top: 15%;
        right: 0;
        width: 60px;
        height: auto;
        background-color: #f8f9fa;
        z-index: 3;
        border-left: 1px solid #ddd;
          border-radius: 10px;

      }
      .tab-button {
        display: block;
        width: 100%;
        padding: 10px;
        border: none;
        background: none;
        cursor: pointer;
        text-align: center;
      }
      .tab-button:hover {
        background-color: #e2e6ea;
      }
      
    "))
  ),
  
  #selector de idioma arriba/izq
  div(
    style = "
      position: absolute;
      top: 10px;
      /* Allargo un po’ a sinistra: se vuoi che il select resti esattamente a 70px,
         abbassa left a (70 + metà larghezza logo). Te lo mostro più avanti. */
      left: 50px;
      z-index: 9999;
      display: flex;
      align-items: center;
      /* gap regola lo spazio orizzontale fra img e select */
      gap: 5px;
    ",
    # 1) Logo con altezza controllata
    tags$img(
      src    = "IRIS.png",     
      height = "30px",          # la alzi/bassi finché non è in linea col select
      style  = "
        display: block;
        /* Se il bordo del select è alto, puoi spostare l’immagine leggermente: */
        margin-bottom: 10px;    /* regola fino a che non è allineato */
      ",
      alt = "Logo IRIS"
    ),
    selectInput(
      inputId = "lang",
      label   = NULL,
      choices = list("IT" = "it", "EN" = "en", "ES" = "es", "CA" = "ca"),
      selected = "ca",
      width   = "80px"
    ),
    actionButton(
      inputId = "infoBtn",
      label   = NULL,
      icon    = icon("info-circle"),
      style  =  paste ("display: block;
        /* Se il bordo del select è alto, puoi spostare l’immagine leggermente: */
        margin-bottom: 16px;    /* regola fino a che non è allineato */
      "  ),
      title = ""
    )
    
  ),
  
  # Main full-screen map
  leafletOutput("map", width = "100%", height = "100%"),
  
  #panel de fecha y localidad
  absolutePanel(
    id = "controlsPanel",
    bottom = 20, left = 10, width = 200, draggable = TRUE,
    style = "z-index: 9999;",
    div(
      style = "background-color: white; padding: 15px; border-radius: 10px; box-shadow: 0px 2px 8px rgba(0,0,0,0.2);",
      uiOutput("localitaSelect18"),
      uiOutput("customDateControls")
    )
  ),
  hidden(
    div(
      id = "mapButtonOverlay",
      style = "position:absolute; top:20px; right:15px; z-index:10; background:rgba(255,255,255,0.9); border-radius:6px; box-shadow:0 2px 6px rgba(0,0,0,0.3); cursor:pointer;",
      actionButton("showMapOnly", "✖", 
                   style = "color: red; background: transparent; border: none; 
                          font-size: 10px; cursor: pointer;",
                   title = "Chiudi")      )
  ),
  
  # Vertical tab buttons on the side
  div(class = "vertical-tabs",
      actionButton("trendTempBtn", "...", class = "tab-button", title = "Temperature"),
      actionButton("trendDiagnBtn", "...", class = "tab-button", title = "Diagnosi"),
      actionButton("trendSexBtn", "...", class = "tab-button", title = "Sesso"),
      actionButton("trendSex2Btn", "...", class = "tab-button", title = "Sesso Trend"),
      actionButton("trendAgeBtn", "...", class = "tab-button", title = "Età"),
      actionButton("trendPostalBtn", "...", class = "tab-button", title = "Codice Postale"),
      actionButton("trendActBtn", "...", class = "tab-button", title = "Actividad"),
      
  ),
  
  # Sidebar that shows content
  div(id = "sidebar", uiOutput("sidebarContent")),
  
  
)



# ========================================================================================================================================================
#                                                                      SERVER
# ========================================================================================================================================================


server <- function(input, output, session) {
  
  ############################### DATE SELECTOR ##################################
  
  min_date <- as.Date(min(T_max_long$Fecha), format = '%Y-%m-%d')
  max_date <- as.Date(max(T_max_long$Fecha), format = '%Y-%m-%d')
  
  selected_range <- reactiveValues(
    start = min_date,
    end = max_date
  )
  
  output$customDateControls <- renderUI({
    date_choices <- format(seq(min_date, max_date, by = "day"), "%d-%m-%Y")
    
    tagList(
      chooseSliderSkin("Flat"),
      sliderTextInput(
        inputId = "dateSliderText",
        label = t("selectDateRange", input$lang),
        choices = date_choices,
        selected = format(c(selected_range$start, selected_range$end), "%d-%m-%Y"),
        grid = FALSE
      ),
      dateRangeInput(
        "dateRangePicker",
        label     = t("selectDatesManually", input$lang),
        start = selected_range$start,
        end = selected_range$end,
        min = min_date,
        max = max_date,
        format = "dd-mm-yyyy",
        separator = " - "
      )
    )
  })
  
  # Sync sliderTextInput -> dateRangeInput
  observeEvent(input$dateSliderText, {
    # Convert from string format "%d-%m-%Y" to Date
    dates <- as.Date(input$dateSliderText, format = "%d-%m-%Y")
    selected_range$start <- dates[1]
    selected_range$end <- dates[2]
    
    updateDateRangeInput(session, "dateRangePicker",
                         start = dates[1],
                         end = dates[2])
  })
  
  # Sync dateRangeInput -> sliderTextInput
  observeEvent(input$dateRangePicker, {
    dates <- input$dateRangePicker
    selected_range$start <- dates[1]
    selected_range$end <- dates[2]
    
    # Format dates back to "%d-%m-%Y" for sliderTextInput
    updateSliderTextInput(session, "dateSliderText",
                          selected = format(c(dates[1], dates[2]), "%d-%m-%Y"))
  })
  
  # Debug print selected range
  output$selectedRange <- renderPrint({
    list(Start = selected_range$start, End = selected_range$end)
  })
  
  
  ############################### DATE SELECTOR END #############################
  
  ############################### TABS ############################################
  
  # Track the selected trend tab
  activeTrendTab <- reactiveVal(NULL)
  
  # When a tab button is clicked, update and show sidebar
  observeEvent(input$trendTempBtn,  { activeTrendTab("temp");  shinyjs::show("sidebar"); shinyjs::show("mapButtonOverlay"); shinyjs::show("infoB") })
  observeEvent(input$trendDiagnBtn, { activeTrendTab("diagn"); shinyjs::show("sidebar"); shinyjs::show("mapButtonOverlay"); shinyjs::show("infoB") })
  observeEvent(input$trendSexBtn,   { activeTrendTab("sex");   shinyjs::show("sidebar"); shinyjs::show("mapButtonOverlay"); shinyjs::show("infoB") })
  observeEvent(input$trendSex2Btn,  { activeTrendTab("sex2");  shinyjs::show("sidebar"); shinyjs::show("mapButtonOverlay"); shinyjs::show("infoB") })
  observeEvent(input$trendAgeBtn,   { activeTrendTab("age");   shinyjs::show("sidebar"); shinyjs::show("mapButtonOverlay"); shinyjs::show("infoB") })
  observeEvent(input$trendPostalBtn,{ activeTrendTab("postal");shinyjs::show("sidebar"); shinyjs::show("mapButtonOverlay"); shinyjs::show("infoB") })
  observeEvent(input$trendActBtn,   { activeTrendTab("act");shinyjs::show("sidebar"); shinyjs::show("mapButtonOverlay"); shinyjs::show("infoB") })
  
  observe({
    if (identical(activeTrendTab(), "temp")) {
      shinyjs::disable("controlsPanel")
      shinyjs::addClass(selector = "#controlsPanel", class = "grayed-out")
    } else {
      shinyjs::enable("controlsPanel")
      shinyjs::removeClass(selector = "#controlsPanel", class = "grayed-out")
    }
  })
  
  # Sidebar panel content
  output$sidebarContent <- renderUI({
    switch(activeTrendTab(),
           
           # nella parte “temp” della tua UI:
           "temp" = tagList(
             actionButton(
               "infoTemp",
               label   = NULL,
               icon    = icon("info-circle"),
               style = "position:absolute; top:50px; right:10px; z-index:9999; background:transparent; border:none; font-size:18px; margin-bottom:10px; cursor:pointer;"
             ),
             
             h4(textOutput("temperTabTitle")),
             uiOutput("selectPlotTypeTemp"),
             
             uiOutput("sliderTemp"),
             
             # 1) DIV flessibile che contiene: selectLocalita2, toggleCodpost2 e toggleSmooth
             div(
               style = "display: flex; align-items: center; gap: 5px; margin-bottom: 5px;",
               uiOutput("selectLocalita2"),
               actionButton(
                 inputId = "toggleCodpost2",
                 label   = NULL,
                 icon    = icon("plus"),
                 style   = "padding: 4px 8px; font-size: 10px; margin-top: 9px;"
               ),
               actionButton(
                 inputId = "toggleSmooth",
                 label   = NULL,
                 icon    = icon("wave-square"),
                 title   = t("helpTextLento", input$lang),
                 style   = "padding: 4px 8px; font-size: 10px; margin-top: 9px;"
               )
             ),
             
             # 2) Container nascosto per il select dei codici postali (nella stessa logica di prima)
             hidden(
               div(
                 id = "selectcodpost2_container",
                 uiOutput("selectcodpost2")
               )
             ),
             
             # 3) Menu a tendina “Trend vs Anomalie” (come prima)
             #uiOutput("selectPlotTypeTemp"),
             #br(),
             
             # 4) I due grafici Plotly condizionali
             conditionalPanel(
               condition = "input.plotTypeTemp == 'trend'",
               plotlyOutput("trendTemperatura", height = "350px", width = "98%")
             ),
             conditionalPanel(
               condition = "input.plotTypeTemp == 'anomalie'",
               plotlyOutput("anomTemperatura", height = "350px", width = "100%")
             )
           ),
           
           
           "diagn" = tagList(
             actionButton(
               "infoDiagn",
               label   = NULL,
               icon    = icon("info-circle"),
               style = "position:absolute; top:50px; right:1px; z-index:9999;",
               
               style   = "background:transparent; border:none; font-size:18px; margin-bottom:10px; cursor:pointer;"
             ),
             h4(textOutput("diagnTabTitle")),
             uiOutput("diagnViewUI"),
             uiOutput("cumTrendDateUI"),
             uiOutput("diagnTabContent"),
           ),
           "act" = tagList(
             actionButton(
               "infoAct",
               label   = NULL,
               icon    = icon("info-circle"),
               style = "position:absolute; top:50px; right:10px; z-index:9999;",
               
               style   = "background:transparent; border:none; font-size:18px; margin-bottom:10px; cursor:pointer;"
             ),
             h4(textOutput("diagnActTitle")),
             uiOutput("diagnActUI"),
             uiOutput("actTrendDateUI"),
             uiOutput("ActTabContent"),
             conditionalPanel(
               condition = "input.ActView == 'weeklyAct'",
               
               # secondo helpText senza margini extra
               div(
                 style = "
                        font-size:12px;
                        white-space: pre-line;
                        line-height:1;
                        padding-top:0px;        
                        margin:0;",
                 htmlOutput("helpTextAct3")
               )
             )
           ),
           "sex" = tagList(
             actionButton(
               "infoSex",
               label   = NULL,
               icon    = icon("info-circle"),
               style = "position:absolute; top:50px; right:10px; z-index:9999;",
               
               style   = "background:transparent; border:none; font-size:18px; margin-bottom:10px; cursor:pointer;"
             ),
             h4(textOutput("sexxTabTitle")),
             uiOutput("selectSessoMode"),
             plotOutput("distSessoComplete")
           ),
           
           "sex2" = {
             # Ottieni il range di date; qui usiamo quello fisso, oppure dentro reactive se necessario
             rng <- date_range_fixed
             # Se preferisci usare reactive, sostituisci con: rng <- date_range_reactive()
             
             tagList(
               actionButton(
                 "infoSex2", label = NULL, icon = icon("info-circle"),
                 style = "position:absolute; top:50px; right:10px; z-index:9999; background:transparent; border:none; font-size:18px; margin-bottom:10px; cursor:pointer;"
               ),
               h4(textOutput("sexoTabTitle")),
               selectInput(
                 "diagnosis",
                 label   = t("selectDiagnosis", input$lang),
                 choices = setNames(choices_filtrate, translate_tipo_P(choices_filtrate, input$lang)),
                 selected = choices_filtrate,
                 multiple = TRUE
               ),
               # Slider basato su rng[1], rng[2]
               div(class = "custom-slider-sex",
                   sliderInput("dateRangeSex",
                               label     = t("periodoOsservazioneSex", input$lang),
                               min       = rng[1],
                               max       = rng[2],
                               value     = rng,
                               timeFormat = "%Y-%m-%d"
                   )
               ),
               plotlyOutput("trendSessoDiagnosi")
             )
           },
           
           "age" = tagList(
             actionButton(
               "infoAge",
               label   = NULL,
               icon    = icon("info-circle"),
               style = "position:absolute; top:50px; right:10px; z-index:9999;",
               
               style   = "background:transparent; border:none; font-size:18px; margin-bottom:10px; cursor:pointer;"
             ),
             h4(textOutput("edadsTabTitle")),
             uiOutput("selectEdadMode"),
             plotOutput("graficoEta")
           ),
           
           "postal" = tagList(
             actionButton(
               "infoPostal", 
               label = NULL,
               icon  = icon("info-circle"),
               style = "position:absolute; top:50px; right:10px; z-index:9999; background:transparent; border:none; font-size:18px; margin-bottom:10px; cursor:pointer;"
             ),
             
             ## Titolo del pannello – diventa dinamico
             h4(textOutput("postalTabTitle")),
             
             # Selettore diagnostico
             uiOutput("postalCategoryUI"),  
             htmlOutput("postalNote"),
             
             # Dropdown per scegliere tipo di grafico, con etichette tradotte
             selectInput(
               inputId = "postal_plot_type",
               label   = t("selectPostalPlotType", input$lang),
               choices = setNames(
                 c("standard", "mirror"),
                 c(
                   t("postal_plot_standard", input$lang),
                   t("postal_plot_mirror",  input$lang)
                 )
               ),
               selected = "standard"
             ),
             
             # Se “standard”, mostro postalPlot + testo di aiuto
             conditionalPanel(
               condition = "input.postal_plot_type == 'standard'",
               plotlyOutput("postalPlot", height = "350px"),
               ## Se hai un testo fisso di aiuto, usa textOutput per renderlo dinamico
               textOutput("helpTextGrafInter2")
             ),
             
             # Se “mirror”, mostro mirrorBarPlot + mirrorTable
             conditionalPanel(
               condition = "input.postal_plot_type == 'mirror'",
               plotlyOutput("mirrorBarPlot", height = "190"),
               br(),
               div(
                 style = "font-size:10px; max-height:300px; overflow-y:auto; padding: 0 5px;",
                 DT::dataTableOutput("mirrorTable")
               )
             )
           )
           
    )
  })
  
  
  ########################### TABS END #############################
  observeEvent(input$showMapOnly, {
    shinyjs::hide("sidebar")
    shinyjs::hide("mapButtonOverlay")
    activeTrendTab(NULL)   # <— resetto il tab attivo
  })
  observeEvent(input$showTrends, {
    output$sidebarContent <- renderUI({ tagList(
      h3(t("trendTabTitle", input$lang)),
      uiOutput("trendTabsUI")
    ) })
    shinyjs::show("sidebar"); shinyjs::show("mapButtonOverlay")
  })
  observeEvent(input$closeSB, {
    shinyjs::hide("sidebar")
    shinyjs::hide("closeButtonOverlay")
  })
  observeEvent(input$closeSB, {
    output$sidebarContent <- renderUI({ tagList(
      h3(t("trendTabTitle", input$lang)),
      uiOutput("trendTabsUI")
    ) })
    shinyjs::show("sidebar"); shinyjs::show("closeButtonOverlay")
  })
  observeEvent(input$showMapOnly, {
    shinyjs::hide("sidebar")
    shinyjs::hide("infoB")
  })
  # ========================================================================================================================================================
  #                                                                      traduzioni
  # ========================================================================================================================================================
  output$localitaSelect18 <- renderUI({
    req(input$lang)   # per far sì che si riesegua al cambio lingua
    
    selectInput(
      inputId  = "localita",
      label    = t("selectLocation", input$lang),  # la chiave di traduzione
      choices  = names(localita_codici_postali),
      selected = NULL,
      multiple = TRUE
    )
  })
  
  output$helpTextICD <- renderUI({
    HTML(helpTextICD10[[ input$lang ]])
  })
  output$helpTextICDdiab <- renderUI({
    HTML(helpTextICD11[[ input$lang ]])
  })
  output$helpTextICDambos <- renderUI({
    HTML(helpTextICDamb[[ input$lang ]])
  })
  output$postalTabTitle <- renderText({
    # Per esempio, aggiungi una chiave "postalTabTitle" al dizionario
    t("postalGenTitle", input$lang)
  })
  
  # 2.1 – Quando cambia la lingua, aggiorna dinamicamente il tooltip della “i”
  observeEvent(input$lang, {
    # testi per ogni lingua
    titles <- list(
      it = "Mostra informazioni",
      en = "Show information",
      es = "Mostrar información",
      ca = "Mostra informació"
    )
    # recupera il testo corretto
    t <- titles[[input$lang]]
    # usa shinyjs per settare l’attributo title sul pulsante
    runjs(sprintf("$('#infoBtn').attr('title', '%s');", t))
  })
  
  # 2.2 – All’avvio, fa partire l’inizializzazione del title una prima volta
  observe({
    # trigger manuale
    input$lang
    # richiama lo stesso codice (oppure spostalo in una funzione)
    titles <- list(
      it = "Mostra informazioni",
      en = "Show information",
      es = "Mostrar información",
      ca = "Mostra informació"
    )
    t <- titles[[input$lang]]
    runjs(sprintf("$('#infoBtn').attr('title', '%s');", t))
  })
  
  # 2.3 – Quando clicchi, mostra una modal con contenuto localizzato
 observeEvent(input$infoBtn, {
    # contenuti per lingua
    contents <- list(
      it = HTML("
        <ul>
          <li> Generale include tutti i pazienti globali dello studio</li>
          <li> Per cancellare una selezione dal menù a tendina selezionarla e premere canc</li>
          <li> Zona di Studio include tutte le municipalità</li>
          <li> La mappa è interattiva, cliccando su una zona si possono avere informazioni più specifiche.</li>
          <li> La legenda si svuota quando si seleziona solo una località (non si possono fare confronti)</li>
          </ul>
             <br>
             <p>© B2SLab (UPC). All rights reserved. 
For more information on the web page please contact: leonardo.andrea.marcon@estudiantat.upc.edu, blanca.alaejos@upc.edu</p>
             <a href='https://b2slab.upc.edu/' target='_blank'>B2S Lab: clicca qui per maggiori informazioni</a>

  "),
      en = HTML("
        <ul>
          <li> General includes all global patients in the study</li>
          <li> To delete a selection from the dropdown menu, select it and press delete</li>
          <li> The Study Area includes all municipalities</li>
          <li> The map is interactive; clicking on an area provides more specific information.</li>
          <li> The legend clears when only one location is selected (comparisons are not possible)</li>
        </ul>
         <br>
             <p>© B2SLab (UPC). All rights reserved. 
For more information on the web page please contact: leonardo.andrea.marcon@estudiantat.upc.edu, blanca.alaejos@upc.edu</p>
             <a href='https://b2slab.upc.edu/' target='_blank'>B2S Lab: Click here for more information</a>
      "),
      es = HTML("
        <ul>
          <li> General incluye a todos los pacientes globales del estudio</li>
          <li> Para eliminar una selección del menú desplegable, selecciónala y presiona suprimir</li>
          <li> La Zona de Estudio incluye todos los municipios</li>
          <li> El mapa es interactivo; al hacer clic en una zona se obtiene información más específica.</li>
          <li> La leyenda se vacía cuando se selecciona solo una localidad (no se pueden hacer comparaciones)</li>
        </ul>
         <br>
             <p>© B2SLab (UPC). All rights reserved. 
For more information on the web page please contact: leonardo.andrea.marcon@estudiantat.upc.edu, blanca.alaejos@upc.edu</p>
             <a href='https://b2slab.upc.edu/' target='_blank'>B2S Lab: Haz clic aquí para más información</a>
      "),
      ca = HTML("
        <ul>
          <li> General inclou tots els pacients globals de l'estudi</li>
          <li> Per suprimir una selecció del menú desplegable, selecciona-la i prem suprimir</li>
          <li> La Zona d'Estudi inclou tots els municipis</li>
          <li> El mapa és interactiu; en fer clic en una zona s'obté informació més específica.</li>
          <li> La llegenda s'esborra quan se selecciona només una localitat (no es poden fer comparacions)</li>
        </ul>
         <br>
             <p>© B2SLab (UPC). All rights reserved. 
For more information on the web page please contact: leonardo.andrea.marcon@estudiantat.upc.edu, blanca.alaejos@upc.edu</p>
             <a href='https://b2slab.upc.edu/' target='_blank'>B2S Lab: Fes clic aquí per a més informació</a>
      ")
    )
    showModal(
      modalDialog(
        title      = switch(input$lang,
                            it = "Informazioni",
                            en = "Information",
                            es = "Información",
                            ca = "Informació"),
        contents[[input$lang]],
        easyClose  = TRUE,
        footer     = modalButton(
          switch(input$lang,
                 it = "Chiudi",
                 en = "Close",
                 es = "Cerrar",
                 ca = "Tanca")
        )
      )
    )
  })
  output$helpTextDesc <- renderText({
    t("helpTextDesc", input$lang)
  })
  output$helpTextAct1 <- renderText({
    t("helpTextAct2", input$lang)
  })
  output$helpTextAct3 <- renderText({
    t("helpTextAct3", input$lang)
  })
  output$helpTextAct4 <- renderText({
    t("helpTextAct4", input$lang)
  })
  output$helpTextGrafInter2 <- renderText({
    t("helpTextGrafInter2", input$lang)
  })
  # — INFO per Tab Temperatura —
  observeEvent(input$infoTemp, {
    showModal(modalDialog(
      title = switch(input$lang,
                     it = "Info Temperatura", en = "Temperature Info",
                     es = "Info Temperatura", ca = "Informació Temperatura"
      ),
      HTML(switch(input$lang,
                  it = "
                <ul>
                  <li>Si possono scegliere più località, codici postali o loro combinazioni</li>
                  <li>Si possono deselezionare etichette direttamente dalla legenda del grafico premendoci sopra</li>
                  <li>Il grafico della Tendenza della Temperatura rappresenta l'andamento per il periodo e località scelta della temperatura massima diaria</li>
                  <li>Il grafico delle Anomalie Settimanali rappresenta, in rosso le temperature più alte e in azzurro quelle più basse, rispetto alla media locale per il periodo scelto</li>
                  <li>Il pulsante (+) dà la possibilità di scegliere codici postali specifici</li>
                  <li>Il simbolo dell'ondina permette di sovrapporre al grafico l'andamento levigato (smoothed) delle temperature</li>
                </ul>
                ",
                  en = "
                <ul>
                  <li>You can select multiple locations, postal codes, or their combinations</li>
                  <li>You can deselect labels directly from the chart legend by clicking on them</li>
                  <li>The Temperature Trend chart shows the daily maximum temperature over the selected period and location</li>
                  <li>The Weekly Anomaly chart shows, in red, higher-than-average and, in blue, lower-than-average temperatures for the chosen period</li>
                  <li>The (+) button allows selection of specific postal codes</li>
                  <li>The wave icon overlays a smoothed temperature trend line on the chart</li>
                </ul>
                ",
                  es = "
                <ul>
                  <li>Se pueden seleccionar varias localidades, códigos postales o sus combinaciones</li>
                  <li>Se pueden deseleccionar etiquetas directamente desde la leyenda del gráfico haciendo clic en ellas</li>
                  <li>El gráfico de Tendencia de Temperatura representa la temperatura máxima diaria durante el período y localidad seleccionados</li>
                  <li>El gráfico de Anomalías Semanales muestra, en rojo, temperaturas más altas y, en azul, más bajas respecto a la media local del período elegido</li>
                  <li>El botón (+) permite seleccionar códigos postales específicos</li>
                  <li>El símbolo de la onda permite superponer al gráfico la tendencia suavizada (smoothed) de las temperaturas</li>
                </ul>
                ",
                  ca = "
                <ul>
                  <li>Es poden seleccionar diverses localitats, codis postals o les seves combinacions</li>
                  <li>Es poden desseleccionar etiquetes directament des de la llegenda del gràfic fent-hi clic</li>
                  <li>El gràfic de Tendència de Temperatura mostra la temperatura màxima diària durant el període i la localitat seleccionats</li>
                  <li>El gràfic d'Anomalies Setmanals mostra, en vermell, temperatures més altes i, en blau, més baixes respecte a la mitjana local del període escollit</li>
                  <li>El botó (+) permet seleccionar codis postals específics</li>
                  <li>El símbol de l’onada permet superposar al gràfic la tendència suavitzada (smoothed) de les temperatures</li>
                </ul>
                "
      )),
      easyClose = TRUE,
      footer = modalButton(switch(input$lang,
                                  it = "Chiudi", en = "Close", es = "Cerrar", ca = "Tanca"
      ))
    ))
  })
  
  ### INFO ACTIVIDAD
  observeEvent(input$infoAct, {
    showModal(modalDialog(
      title = switch(input$lang,
                     it = "Info Attività", en = "Activity Info",
                     es = "Info Actividad", ca = "Informació Activitat"
      ),
      HTML(switch(input$lang,
                  it = "
                  <ul>
                    <li> I grafici sono interattivi, passando il mouse sopra si possono avere informazioni più specifiche</li>
                    <li> Si possono deselezionare etichette direttamente dalla legenda del grafico premendoci sopra</li>
                    <li> C'è la possibilità di scegliere \"Tutto il Periodo\"</li>
                    </ul>",
                  en = "<ul>
                    <li> The charts are interactive; hovering with the mouse reveals more detailed information</li>
                    <li> You can deselect labels directly from the chart legend by clicking on them</li>
                    <li> There is the option to select \"Entire Period\"</li>
                    </ul>",
                  es = "<ul>
                    <li> Los gráficos son interactivos; al pasar el ratón por encima se obtiene información más específica</li>
                    <li> Se pueden deseleccionar etiquetas directamente desde la leyenda del gráfico haciendo clic en ellas</li>
                    <li> Existe la posibilidad de seleccionar \"Todo el Período\"</li>
                    </ul>",
                  ca = "<ul>
                    <li> Els gràfics són interactius; si es passa el cursor per sobre es mostren dades més específiques</li>
                    <li> Es poden desseleccionar etiquetes directament des de la llegenda del gràfic fent-hi clic</li>
                    <li> Hi ha la possibilitat de seleccionar \"Tot el Període\"</li>
                    </ul>"
      )),
      easyClose = TRUE,
      footer = modalButton(switch(input$lang,
                                  it = "Chiudi", en = "Close", es = "Cerrar", ca = "Tanca"
      ))
    ))
  })
  # — INFO per Tab Diagnosi —
  observeEvent(input$infoDiagn, {
    showModal(modalDialog(
      title = switch(input$lang,
                     it = "Info Diagnosi", en = "Diagnosis Info",
                     es = "Info Diagnósticos", ca = "Informació Diagnòstics"
      ),
      HTML(switch(input$lang,
                  it = "
                <ul>
                  <li>I piechart rappresentano le malattie in codice ICD-10 più o meno comuni tra i pazienti dello studio e le traduzioni dei rispettivi codici</li>
                  <li>Il grafico Trend giornaliero cumulativo mostra l'andamento dei pazienti, indicando per esempio il cambio del tipo di paziente da \"cardiovascolare\" a \"entrambi\" quando sorge una diagnosi di \"diabete\". In questo esempio, un paziente prima considerato \"cardiovascolare\" diventerà \"entrambi\", togliendolo dal gruppo \"cardiovascolare\"</li>
                </ul>
                ",
                  en = "
                <ul>
                  <li>The pie charts represent the more or less common ICD-10 coded diseases among study patients and the translations of those codes</li>
                  <li>The cumulative daily trend chart shows the evolution of patients, illustrating for example the change in patient type from \"cardiovascular\" to \"both\" when a diagnosis of \"diabetes\" occurs. In this case, a patient previously considered \"cardiovascular\" will become \"both\", and will be removed from the \"cardiovascular\" group</li>
                </ul>
                ",
                  es = "
                <ul>
                  <li>Los gráficos circulares representan las enfermedades codificadas en ICD-10 más o menos comunes entre los pacientes del estudio y las traducciones de dichos códigos</li>
                  <li>El gráfico de tendencia diaria acumulativa muestra la evolución de los pacientes, ilustrando por ejemplo el cambio de tipo de paciente de \"cardiovascular\" a \"ambos\" cuando aparece un diagnóstico de \"diabetes\". En este caso, un paciente previamente considerado \"cardiovascular\" pasará a ser \"ambos\", y será eliminado del grupo \"cardiovascular\"</li>
                </ul>
                ",
                  ca = "
                <ul>
                  <li>Els gràfics de sectors representen les malalties codificades amb CIE-10 més o menys comunes entre els pacients de l'estudi i les traduccions dels codis respectius</li>
                  <li>El gràfic de tendència diària acumulativa mostra l'evolució dels pacients, mostrant per exemple el canvi de tipus de pacient de \"cardiovascular\" a \"ambdós\" quan es diagnostica \"diabetis\". En aquest cas, un pacient considerat prèviament \"cardiovascular\" passarà a ser \"ambdós\" i es retirarà del grup \"cardiovascular\"</li>
                </ul>
                "
      )),
      easyClose = TRUE,
      footer = modalButton(switch(input$lang,
                                  it = "Chiudi", en = "Close", es = "Cerrar", ca = "Tanca"
      ))
    ))
  })
  
  
  # — INFO per Tab Sesso —
  observeEvent(input$infoSex, {
    showModal(modalDialog(
      title = switch(input$lang,
                     it = "Info Sesso", en = "Sex Info",
                     es = "Info Sexo", ca = "Informació Sexe"
      ),
      HTML(switch(input$lang,
                  it = "<p>I grafici rappresentano la distribuzione di sesso generale (di tutta la popolazione dello studio) e per diagnostico specifico.</p>",
                  en = "<p>The charts represent the general sex distribution (of the entire study population) and by specific diagnosis.</p>",
                  es = "<p>Los gráficos representan la distribución general por sexo (de toda la población del estudio) y por diagnóstico específico.</p>",
                  ca = "<p>Els gràfics representen la distribució general per sexe (de tota la població de l'estudi) i per diagnòstic específic.</p>"
      )),
      easyClose = TRUE,
      footer = modalButton(switch(input$lang,
                                  it = "Chiudi", en = "Close", es = "Cerrar", ca = "Tanca"
      ))
    ))
  })
  
  
  # — INFO per Tab Trend Sesso/Diagnosi —
  observeEvent(input$infoSex2, {
    showModal(modalDialog(
      title = switch(input$lang,
                     it = "Info Trend Sesso", en = "Sex Trend Info",
                     es = "Info Tendencia Sexo", ca = "Informació Tendència Sexe"
      ),
      HTML(switch(input$lang,
                  it = "<ul>
                        <li> Scelta multipla di diagnosi per confrontare</li>
                      </ul>
                      <p>Il grafico mostra l'andamento per sesso dei pazienti nel tempo. Quello che si visualizza sono le diagnosi singole, quindi lo stesso paziente può essere presente più volte se nel corso del tempo ha avuto diverse diagnosi.</p>",
                  en = "<ul>
                        <li> Select multiple diagnoses to compare</li>
                      </ul>
                      <p>The chart shows the sex-based trend of patients over time. It displays individual diagnoses, so the same patient may appear multiple times if they received different diagnoses over time.</p>",
                  es = "<ul>
                        <li> Selección múltiple de diagnósticos para comparar</li>
                      </ul>
                      <p>El gráfico muestra la evolución por sexo de los pacientes a lo largo del tiempo. Se visualizan los diagnósticos individuales, por lo tanto, un mismo paciente puede aparecer varias veces si ha recibido diferentes diagnósticos a lo largo del tiempo.</p>",
                  ca = "<ul>
                        <li> Selecció múltiple de diagnòstics per comparar</li>
                      </ul>
                      <p>El gràfic mostra la tendència per sexe dels pacients al llarg del temps. Es mostren els diagnòstics individuals, per tant, un mateix pacient pot aparèixer diverses vegades si ha rebut diferents diagnòstics amb el pas del temps.</p>"
      )),
      easyClose = TRUE,
      footer = modalButton(switch(input$lang,
                                  it = "Chiudi", en = "Close", es = "Cerrar", ca = "Tanca"
      ))
    ))
  })
  
  
  # — INFO per Tab Età —
  observeEvent(input$infoAge, {
    showModal(modalDialog(
      title = switch(input$lang,
                     it = "Info Età", en = "Age Info",
                     es = "Info Edad", ca = "Informació Edat"
      ),
      HTML(switch(input$lang,
                  it = "<p>I grafici rappresentano la distribuzione d'età generale (di tutta la popolazione dello studio) e per diagnostico specifico.</p>",
                  en = "<p>The charts represent the general age distribution (of the entire study population) and by specific diagnosis.</p>",
                  es = "<p>Los gráficos representan la distribución de edad general (de toda la población del estudio) y por diagnóstico específico.</p>",
                  ca = "<p>Els gràfics representen la distribució d'edat general (de tota la població de l'estudi) i per diagnòstic específic.</p>"
      )),
      easyClose = TRUE,
      footer = modalButton(switch(input$lang,
                                  it = "Chiudi", en = "Close", es = "Cerrar", ca = "Tanca"
      ))
    ))
  })
  
  
  # — INFO per Tab Codici Postali —
  observeEvent(input$infoPostal, {
    showModal(modalDialog(
      title = switch(input$lang,
                     it = "Info Codici Postali", en = "Postal Codes Info",
                     es = "Info Códigos Postales", ca = "Informació Codis Postals"
      ),
      HTML(switch(input$lang,
                  it = "<ul>
                        <li>I codici postali con pazienti nulli sono stati tolti dalla visualizzazione.</li>
                        <li>'Pazienti per Codice Postale' mostra il numero di pazienti per codice postale (con visualizzazione di municipio per facilità).</li>
                        <li>'Pazienti vs Popolazione' mostra le proporzioni di abitanti nel 2022 della località e i pazienti del periodo di studio di quella località. Inoltre, nella tabella sottostante al grafico è possibile avere maggiori informazioni come Abitanti Totali e percentuale di pazienti.</li>
                      </ul>",
                  en = "<ul>
                        <li>Postal codes with no patients have been removed from the visualization.</li>
                        <li>'Patients by Postal Code' shows the number of patients by postal code (municipality is shown for clarity).</li>
                        <li>'Patients vs Population' displays the 2022 population proportions for each locality and the number of patients during the study period. The table below the chart provides additional details such as total population and patient percentage.</li>
                      </ul>",
                  es = "<ul>
                        <li>Los códigos postales sin pacientes se han eliminado de la visualización.</li>
                        <li>'Pacientes por Código Postal' muestra el número de pacientes por código postal (con visualización de municipio para facilitar).</li>
                        <li>'Pacientes vs Población' muestra las proporciones de habitantes en 2022 de la localidad y los pacientes del período de estudio. Además, en la tabla debajo del gráfico se pueden consultar más detalles como habitantes totales y porcentaje de pacientes.</li>
                      </ul>",
                  ca = "<ul>
                        <li>Els codis postals sense pacients s'han eliminat de la visualització.</li>
                        <li>'Pacients per Codi Postal' mostra el nombre de pacients per codi postal (amb visualització del municipi per facilitar-ho).</li>
                        <li>'Pacients vs Població' mostra les proporcions d'habitants del 2022 de la localitat i els pacients del període d'estudi. A més, a la taula sota el gràfic es poden veure més dades com els habitants totals i el percentatge de pacients.</li>
                      </ul>"
      )),
      easyClose = TRUE,
      footer = modalButton(switch(input$lang,
                                  it = "Chiudi", en = "Close", es = "Cerrar", ca = "Tanca"
      ))
    ))
  })
  
  
  observeEvent(input$lang, {
    # aggiorno i pulsanti laterali
    updateActionButton(session, "trendTempBtn",
                       label = t("trendTempTitle1", input$lang))
    updateActionButton(session, "trendDiagnBtn",
                       label = t("trendDiagnTitle1", input$lang))
    updateActionButton(session, "trendSexBtn",
                       label = t("distSessoTitle1", input$lang))
    updateActionButton(session, "trendSex2Btn",
                       label = t("trendSessoDiagnTitle1", input$lang))
    updateActionButton(session, "trendAgeBtn",
                       label = t("etaMediaTitle1", input$lang))
    updateActionButton(session, "trendPostalBtn",
                       label = t("postalGenTitle1", input$lang))
    updateActionButton(session, "trendActBtn",
                       label = t("activityChartsTitle1", input$lang))
    
    # aggiorno anche il bottone Info globale
    runjs(sprintf("$('#infoBtn').attr('title','%s')",
                  t("helpTextGeneral", input$lang)))
  })
  
  
  ######################## MAP ########################
  
  map_temp_reactive <- reactive({
    req(selected_range$start, selected_range$end)
    
    T_max_long %>%
      filter(Fecha >= selected_range$start, Fecha <= selected_range$end) %>%
      mutate(codpost = as.character(codpost)) %>%
      group_by(codpost) %>%
      summarise(avgTmax_slider = mean(Tmax, na.rm = TRUE), .groups = "drop")
  })
  
  
  
  map_data_by_date <- reactive({
    req(map_temp_reactive())
    map_data <- map_temp_reactive() %>%
      mutate(codpost = as.character(codpost))
    
    spatial_data <- barcelona_map_valid %>%
      mutate(codpost = as.character(codpost))
    
    if (is.null(input$localita) || length(input$localita) == 0) {
      joined_data <- spatial_data %>%
        left_join(map_data, by = "codpost")
    } else {
      selected_codpost <- unlist(localita_codici_postali[input$localita])
      joined_data <- spatial_data %>%
        filter(codpost %in% selected_codpost) %>%
        left_join(map_data, by = "codpost")
    }
    
    return(joined_data)
  })
  
  
  output$debugFilteredData <- renderPrint({
    filtered_data <- map_data_by_date()
    pal <- colorNumeric("YlOrRd", domain = filtered_data$avgTmax_slider, na.color = "#ccc")
    print(sf::st_geometry_type(filtered_data))
    print(attr(filtered_data, "sf_column"))  # Should be "geometry"
    
    print(class(filtered_data))
    req(input$localita)
    barcelona_map_valid %>%
      left_join(map_temp_reactive(), by="codpost") %>%
      filter(codpost %in% unlist(localita_codici_postali[input$localita]))
    
    print(unique(unlist(localita_codici_postali[input$localita])))
    print(input$localita)
    
    
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 2.16, lat = 41.56, zoom = 10.5)
  })
  
  observe({
    req(map_data_by_date())
    
    filtered_data <- map_data_by_date()
    pal <- colorNumeric("YlOrRd", domain = filtered_data$avgTmax_slider, na.color = "#ccc")
    
    labels <- paste0(
      "<strong style='font-size:15px;'>", filtered_data$localita, "</strong><br>",
      "<strong>", t("postal_code", input$lang), ": </strong>", filtered_data$codpost, "<br>",
      "<strong>", t("patient_count", input$lang), ": </strong>", filtered_data$poblacion, "<br>",
      "<strong>", t("Agemed", input$lang), " : </strong>", round(filtered_data$avg_age, 1), " ", t("year_month", input$lang), "<br>",
      "<strong>", t("avg_temp", input$lang), ": </strong>", round(filtered_data$avgTmax_slider, 1), " °C<br>",
      "<strong>", t("diagnosis_count", input$lang), ": </strong>", filtered_data$n_diagn
    )
    
    leafletProxy("map", data = filtered_data) %>%
      clearShapes() %>%
      clearControls() %>% 
      addPolygons(
        fillColor = ~pal(avgTmax_slider),
        color = "white",
        weight = 2,
        opacity = 1,
        dashArray = "3",
        fillOpacity = 0.7,
        label = lapply(labels, htmltools::HTML),
        labelOptions = labelOptions(
          direction = "auto",
          opacity = 0.95,
          style = list(
            "background-color" = "white",
            "color" = "black",
            "border-radius" = "6px",
            "box-shadow" = "0 3px 14px rgba(0,0,0,0.4)",
            "padding" = "15px",
            "font-weight" = "normal",
            "font-size" = "12px",
            "border" = "1px solid rgba(0,0,0,0.2)"
          )
        )
      )%>%
      addLegend(
        position  = "topleft",
        pal       = pal,
        values    = filtered_data$avgTmax_slider,
        title     = t("avg_temp", input$lang),
        labFormat = labelFormat(
          suffix = "°C",
          digits = 1        # qui forzi un solo decimale
        ),
        className = "custom-legend"
      )
    
    
  })
  observe({
    req(map_data_by_date())
    assign("filtered_data_debug", map_data_by_date(), envir = .GlobalEnv)
  })
  
  ###################### ACTIVIDAD  ############################
  output$diagnActUI <- renderUI({
    codes  <- c("weeklyAct", "other2")
    labels <- c(
      t("weeklyActTitle", input$lang),
      # t("altroGrafico1Title", input$lang),
      t("otherGrafico2Title", input$lang)
    )
    selectInput(
      "ActView",
      label    = t("selectView", input$lang),  # aggiungi chiave se serve
      choices  = setNames(codes, labels),
      selected = codes[1]
    )
  })
  
  
  output$diagnActTitle <- renderText({
    t("activityChartsTitle", input$lang)  # o altra chiave nel tuo dizionario
  })
  # --- 2) Contenuto dinamico del tab Actividad ---
  output$ActTabContent <- renderUI({
    req(input$ActView)
    switch(input$ActView,
           
           # SE weeklyAct, mostro selectYear + weeklyActPlot
           "weeklyAct" = tagList(
             fluidRow(
               column(
                 width = 4,
                 selectInput(
                   inputId = "weekYear",
                   label   = t("selectYear", input$lang),
                   choices = setNames(
                     c("all", names(df_per_year_list)),
                     c(t("all", input$lang), names(df_per_year_list))
                   ),
                   selected = "all"
                 )
               ),
               column(
                 width = 12,
                 plotlyOutput("weeklyActPlot", height = "300px")
               )
             )
           ),
           
           # SE other1, mostro altroPlot1
           "other1" = plotlyOutput("otherPlot1"),
           
           # SE other2, mostro altroPlot2
           "other2" = plotlyOutput("otherPlot2")
    )
  })
  
  
  #WEEKLY
  output$weeklyActPlot <- renderPlotly({
    req(input$weekYear)
    
    types     <- sort(unique(ds_act_weekly$tipo))
    lab_types <- setNames(lapply(types, function(x) t(x, input$lang)), types)
    
    # Palette di colori per tipo (assicurati che i nomi corrispondano ai valori di tipo)
    tipo_colors <- c(
      "Control"        = "#1f77b4",
      "Ambos"          = "#ff7f0e",
      "Cardiovascular" = "#2ca02c",
      "Diabetes"       = "#d62728"
    )
    
    p <- plot_ly()
    
    if (input$weekYear == "all") {
      # Barre
      for (tp in types) {
        sub <- ds_act_weekly %>% filter(tipo == tp)
        p <- add_bars(
          p, data = sub, x = ~semana, y = ~total_inter,
          name = lab_types[[tp]],
          marker = list(color = tipo_colors[tp], opacity = 0.7)  # colore + opacità
        )
      }
      # Linea temperatura media
      p <- add_lines(
        p, data = ds_temp_media, x = ~semana, y = ~avg_temp,
        name = t("trendTempand", input$lang), yaxis = "y2",
        mode = "lines", line = list(color = "black", width = 2)
      )
      
    } else {
      # Barre per anno
      yr  <- as.character(input$weekYear)
      dfy <- ds_act_by_year[[yr]]
      for (tp in types) {
        sub <- dfy %>% filter(tipo == tp)
        p <- add_bars(
          p, data = sub, x = ~semana, y = ~total_inter,
          name = lab_types[[tp]],
          marker = list(color = tipo_colors[tp], opacity = 0.7)
        )
      }
      # Linea temperatura per anno
      tmp_year <- ds_temp_media %>%
        filter(lubridate::year(semana) == as.integer(input$weekYear))
      p <- add_lines(
        p, data = tmp_year, x = ~semana, y = ~avg_temp,
        name = t("trendTempand", input$lang), yaxis = "y2",
        mode = "lines", line = list(color = "black", width = 2)
      )
    }
    
    p %>% layout(
      barmode = "stack",
      xaxis = list(title = t("tit3", input$lang)),  # Aggiungi questa riga per il titolo dell'asse X
      yaxis   = list(title = t("eventCount", input$lang)),
      yaxis2  = list(
        overlaying = "y", side = "right",
        title      = t("avg_temp", input$lang),
        showgrid   = FALSE
      ),
      legend = list(
        orientation = "h",
        x = 0.5, 
        xanchor = "center",
        y = -0.4
      ),
      margin = list(b = 100, r=60)
    )
  })
  
  
  ###########################################################
  
  
  act_total_grupos_dia <- act_total_grupos_dia_cp %>%
    mutate(
      semana = floor_date(fecha, unit = "week", week_start = 1),
      year   = year(fecha)
    ) %>%
    group_by(semana, tipo, year) %>%
    summarise(
      interacciones = sum(interacciones),
      .groups = "drop"
    )
  
  # 2) Palette di colori per tipo
  tipo_colors <- c(
    "Control"       = "#1f77b4",
    "Ambos"         = "#ff7f0e",
    "Cardiovascular"= "#2ca02c",
    "Diabetes"      = "#d62728"
  )
  
  # 3) Il renderPlotly
  output$otherPlot2 <- renderPlotly({
    req(act_total_grupos_dia)
    
    fig <- plot_ly(height = 400)
    
    for (tname in unique(act_total_grupos_dia$tipo)) {
      sub <- filter(act_total_grupos_dia, tipo == tname)
      fig <- fig %>%
        add_lines(
          data = sub,
          x    = ~semana,
          y    = ~interacciones,
          name = t_tipo_P(tname, input$lang),     # legenda tradotta
          line = list(color = tipo_colors[[tname]], width = 1)
        )
    }
    
    fig %>%
      layout(
        title = list(
          text = t("otherGrafico2Title", input$lang),
          font = list(size = 20)
        ),
        showlegend = TRUE,
        legend = list(
          title   = list(text = t("tipoPaziente", input$lang)),
          orientation = "h",
          x           = 0.5,
          xanchor     = "center",
          y           = -0.7,
          yanchor     = "top"
        ),
        xaxis = list(
          title       = t("week", input$lang),   # etichetta asse X
          type        = "date",
          tickformat  = "%Y",
          rangeslider = list(visible = TRUE),
          rangeselector = list(
            buttons = list(
              list(count = 1, label = "1m",  step = "month", stepmode = "backward"),
              list(count = 6, label = "6m",  step = "month", stepmode = "backward"),
              list(count = 1, label = "YTD", step = "year",  stepmode = "todate"),
              list(count = 1, label = "1y",  step = "year",  stepmode = "backward"),
              list(step = "all")
            )
          )
        ),
        yaxis = list(
          title         = t("event_count", input$lang),  # etichetta asse Y
          zerolinecolor = "#ffff",
          zerolinewidth = 2,
          gridcolor     = "#ffff"
        ),
        plot_bgcolor = "#e5ecf6",
        margin = list(l = 40, r = 40, b = 120, t = 60)
      )
  })
  
  
  
  
  ###################### DIAGNOSTICS  ############################
  output$diagnViewUI <- renderUI({
    # codici interni
    codes  <- c("pieCardio", "pieDiab", "pieAmbos", "cumTrend")
    # etichette tradotte
    labels <- c(
      t("cardioPieTitle", input$lang),
      t("diabPieTitle",   input$lang),
      t("ambosPieTitle",  input$lang),
      t("cumTrendTitle",  input$lang)
    )
    selectInput(
      "diagnView",
      label    = t("selectDiagnView", input$lang),
      choices  = setNames(codes, labels),  # names = labels, values = codes
      selected = codes[1]
    )
  })
  
  # Contenuto dinamico del tab “Diagnosi”
  # Contenuto dinamico del tab “Diagnosi”
  output$diagnTabContent <- renderUI({
    req(input$diagnView)
    
    # per comodità, dimensioni e offset in variabile
    w <- 6; off <- 0; h <- "300px"; ww <- "500px"
    
    if (input$diagnView == "pieCardio") {
      tagList(
        fluidRow(
          column(
            width = 6,
            plotOutput("pieCardio", height = h, width = ww)
          )
        ),
        fluidRow(
          column(
            width = 12,
            div(
              style = "margin-top: 0px; font-size: 12px; white-space: pre-wrap;",
              htmlOutput("helpTextICD")
            )
          )
        )
      )
    }else if (input$diagnView == "pieDiab") {
      tagList(
        fluidRow(
          column(
            width = 6,
            plotOutput("pieDiab", height = h, width = ww)
          )
        ),
        fluidRow(
          column(
            width = 12,
            div(
              style = "margin-top: 0px; font-size: 12px; white-space: pre-wrap;",
              htmlOutput("helpTextICDdiab")
            )
          )
        )
      )
      
    } else if (input$diagnView == "pieAmbos") {
      tagList(
        fluidRow(
          column(
            width = 6,
            plotOutput("pieAmbos", height = h, width = ww)
          )
        ),
        fluidRow(
          column(
            width = 12,
            div(
              style = "margin-top: 0px; font-size: 12px; white-space: pre-wrap;",
              htmlOutput("helpTextICDambos")
            )
          )
        )
      )
      
    } else if (input$diagnView == "cumTrend") {
      column(12, plotOutput("cumTrendPlot", height = "400px"))
      
      
      
    } else {
      column(12, HTML("<em>Seleziona un grafico dal menu Diagnosi</em>"))
    }
  })
  
  
  
  # 3) Render cumulativo
  
  # UI dinamica per il periodo di cumTrend
  output$cumTrendDateUI <- renderUI({
    req(input$diagnView == "cumTrend")           # solo se cumTrend è attivo
    dr <- agg_daily$giorno                     # tutte le date disponibili
    dateRangeInput(
      inputId   = "trendDateRange",
      label     = t("selectDateRange", input$lang),
      start     = min(dr),
      end       = max(dr),
      language  = input$lang,
      separator = " – "
    )
  })
  
  # Il tuo renderPlot per cumTrend, che adesso usa input$trendDateRange
  output$cumTrendPlot <- renderPlot({
    # assicurati che trendDateRange esista prima di usarlo
    req(input$trendDateRange)
    
    df_plot <- agg_daily %>%
      filter(
        tipo_P != "Control",
        giorno >= input$trendDateRange[1],
        giorno <= input$trendDateRange[2]
      ) %>%
      mutate(
        tipo_label = recode(
          tipo_P,
          Diabetes       = t("Diabetes",       input$lang),
          Cardiovascular = t("Cardiovascular", input$lang),
          Ambos          = t("Ambos",          input$lang)
        )
      )
    
    ggplot(df_plot, aes(x = giorno, y = n, color = tipo_label, group = tipo_label)) +
      geom_line(size = 0.8, alpha = 0.65) +
      scale_x_date(
        date_breaks = "3 months",
        date_labels = "%Y-%m"
      ) +
      labs(
        title = t("cumTrendTitle",   input$lang),
        x     = t("date",            input$lang),
        y     = t("patient_count",   input$lang),
        color = t("tipoPaziente2",    input$lang)
      ) +
      theme_minimal() +
      theme(
        axis.text.x     = element_text(angle = 45, hjust = 1),
        legend.position = "bottom"
      )
  })
  
  
  # Pie‐chart Cardiovascular
  
  output$pieCardio <- renderPlot({
    other_lbl <- t("otherCategory", input$lang)
    
    diags_cardio <- pie_cardio_base %>%
      mutate(icd10_label = ifelse(is_other, other_lbl, icd10)) %>%
      group_by(icd10_label) %>%
      summarise(n = sum(n), percentage = sum(percentage), .groups = "drop")
    
    legend_labels <- paste0(diags_cardio$icd10_label, " - ",
                            diags_cardio$percentage, "% (", diags_cardio$n, ")")
    
    ggplot(diags_cardio, aes(x = "", y = n, fill = icd10_label)) + 
      geom_bar(stat = "identity", color = "#e9ecfe", alpha = 0.9) + 
      scale_y_continuous(expand = c(0, 0)) +
      coord_polar(theta = "y") +
      paletteer::scale_fill_paletteer_d(
        "MoMAColors::Rattner",
        labels = legend_labels
      ) + 
      geom_text(aes(1.8, label = paste0(icd10_label, "\n", percentage, "%")),
                position = position_stack(vjust = 0.5), size = 3) +
      labs(
        title = t("cardioPieTitle", input$lang),
        fill  = t("icd10", input$lang)
      ) +
      theme_void() +
      theme(
        plot.title  = element_text(
          hjust  = 0.5,
          margin = margin(t = 0, r = 0, b = 2, l = 0)
        ),
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")
      )
  })
  
  
  
  
  #pie-diab
  output$pieDiab <- renderPlot({
    other_lbl <- t("otherCategory", input$lang)
    
    diags_diab <- pie_diab_base %>%
      mutate(
        icd10_label = ifelse(is_other, other_lbl, icd10)
      ) %>%
      group_by(icd10_label) %>%
      summarise(
        n          = sum(n),
        percentage = sum(percentage),
        .groups    = "drop"
      )
    
    legend_labels <- paste0(
      diags_diab$icd10_label, " - ",
      diags_diab$percentage, "% (", diags_diab$n, ")"
    )
    
    ggplot(diags_diab, aes(x = "", y = n, fill = icd10_label)) +
      geom_bar(stat = "identity", color = "#e9ecfe", alpha = 0.9) +
      coord_polar(theta = "y") +
      paletteer::scale_fill_paletteer_d(
        "MoMAColors::Rattner",
        labels = legend_labels
      ) +
      geom_text(aes(1.8, label = paste0(icd10_label, "\n", percentage, "%")),
                position = position_stack(vjust = 0.5), size = 3) +
      labs(
        title = t("diabPieTitle", input$lang),
        fill  = t("icd10", input$lang)
      ) +
      theme_void() +
      theme(
        plot.title  = element_text(
          hjust  = 0.5,
          margin = margin(t = 0, r = 0, b = 2, l = 0)
        ),
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")
      )
  })
  
  
  #pie-ambos
  # carica il dataframe preparato
  
  output$pieAmbos <- renderPlot({
    # etichetta “Other” tradotta
    other_lbl <- t("otherCategory", input$lang)
    
    # applica traduzione “Other” e aggrega
    combos <- pie_ambos_base %>%
      mutate(
        combo_label = ifelse(combo_flag, other_lbl, combo_raw)
      ) %>%
      group_by(combo_label) %>%
      summarise(
        n          = sum(n),
        percentage = sum(percentage),
        .groups    = "drop"
      )
    
    # prepara le etichette della legenda
    legend_labels <- paste0(
      combos$combo_label, " - ",
      combos$percentage, "% (", combos$n, ")"
    )
    
    # disegna il pie-chart
    ggplot(combos, aes(x = "", y = n, fill = combo_label)) +
      geom_bar(stat = "identity", color = "#e9ecfe", alpha = 0.9) +
      coord_polar(theta = "y") +
      paletteer::scale_fill_paletteer_d(
        "MoMAColors::Rattner",
        labels = legend_labels,
        limits = combos$combo_label
      ) +
      geom_text(aes(
        1.8,
        label = paste0(percentage, "%")
      ),
      position = position_stack(vjust = 0.5),
      size = 3
      ) +
      labs(
        title = t("ambosPieTitle", input$lang),
        fill  = t("combination",   input$lang)
      ) +
      theme_void() +
      theme(
        plot.title  = element_text(
          hjust  = 0.5,
          margin = margin(t = 0, r = 0, b = 2, l = 0)
        ),
        plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")
      )
  })
  
  
  
  ###############################  TAB SEX ##################################
  output$selectSessoMode <- renderUI({
    # Imposta le opzioni: includi "Generale" e tutte le possibili diagnosi
    # Applichiamo la traduzione per ciascuna opzione
    translated_choices <- c(t("Generale", input$lang),t("diagnTabTitle", input$lang))
    general_input <- c(t("Generale", input$lang))
    selectInput("tipo_Sesso", 
                label = t("selectSessoModeLabel", input$lang),  # Assicurati di avere una traduzione per questo label
                choices = translated_choices,
                selected = general_input,  
                multiple = FALSE)
  })
  
  output$sexxTabTitle <- renderText({
    t("distSessoTitle", input$lang)  # or just "Sex Distribution" if you don't have translation function
  })
  
  output$distSessoComplete <- renderPlot({
    req(input$tipo_Sesso)
    
    if (input$tipo_Sesso == t("Generale", input$lang)) {
      df <- sex_general %>%
        mutate(
          sexo_trad  = sapply(sexo, translate_gender, input$lang),
          # Normalizzo prop in percentuale [0,1]:
          pct        = prop / sum(prop, na.rm = TRUE),
          # Poi creo il label:
          pct_label  = scales::percent(pct, accuracy = 0.1)
        )
      
      ggplot(df, aes(x = "", y = pct, fill = sexo_trad)) +
        geom_bar(stat = "identity", width = 1) +
        geom_text(
          aes(label = pct_label),
          position = position_stack(vjust = 0.5),
          color    = "white",
          fontface = "bold",
          size     = 4
        ) +
        coord_polar(theta = "y") +
        labs(
          title = t("distSessoTitle", input$lang),
          fill  = t("sesso23",       input$lang)
        ) +
        theme_void() +
        theme(
          plot.title  = element_text(hjust = 0.5),
          plot.margin = margin(t = 0, b = 0, unit = "pt")
        )
      
    } else {
      all_diag <- sort(unique(sex_by_diag$tipo_P))
      
      df <- sex_by_diag %>%
        filter(tipo_P %in% all_diag) %>%
        mutate(
          sexo_trad = sapply(sexo, translate_gender, input$lang),
          sexo_trad = factor(sexo_trad, levels = c(
            translate_gender("M", input$lang),  # Donna / Woman / Mujer / Dona
            translate_gender("V", input$lang)   # Uomo / Man / Hombre / Home
          )),
          tipo_trad = translate_tipo_P_new(tipo_P, input$lang)
        ) %>%
        # raggruppo per facet e normalizzo:
        group_by(tipo_trad) %>%
        mutate(
          pct       = prop / sum(prop, na.rm = TRUE),
          pct_label = scales::percent(pct, accuracy = 0.1)
        ) %>%
        ungroup()
      
      ggplot(df, aes(x = "", y = pct, fill = sexo_trad)) +
        geom_bar(stat = "identity", width = 1) +
        geom_text(
          aes(label = pct_label),
          position = position_stack(vjust = 0.5),
          color    = "white",
          fontface = "bold",
          size     = 3
        ) +
        coord_polar(theta = "y") +
        facet_wrap(~ tipo_trad) +
        labs(
          title = t("distSessoDiagTitle", input$lang),
          fill  = t("sesso23",           input$lang)
        ) +
        theme_void() +
        theme(
          plot.title  = element_text(hjust = 0.5),
          plot.margin = margin(t = 0, b = 0, unit = "pt")
        )
      
    }
    
  })
  
  
  ############################### TAB SEX 2 ###############################
  
  output$trendSessoDiagnosi <- renderPlotly({
    req(input$dateRangeSex, input$diagnosis)
    
    # 1) Filtra e prepara i dati
    df_filtered <- df_trend_sex_diag %>%
      filter(
        year_month >= input$dateRangeSex[1],
        year_month <= input$dateRangeSex[2],
        tipo_P %in% input$diagnosis
      ) %>%
      mutate(
        tipo_P_trad = factor(
          tipo_P,
          levels = unique(tipo_P),
          labels = translate_tipo_P(unique(tipo_P), input$lang)
        ),
        sexo_trad = factor(
          sapply(sexo, translate_gender, input$lang),
          levels = c(
            translate_gender("M", input$lang),
            translate_gender("V", input$lang)
          )
        )
      )
    
    # 2) Costruisci il ggplot (nota labs(x = NULL) per togliere “year_month”)
    p <- ggplot(df_filtered,
                aes(x = year_month, y = pct,
                    color = sexo_trad,
                    group = interaction(sexo_trad, tipo_P),
                    text = paste0(
                      format(year_month, "%d %b %Y"), "<br>",
                      scales::percent(pct/100, accuracy = 0.1), "<br>",
                      sexo_trad
                    )
                )
    ) +
      geom_line(size = 0.4) +
      scale_color_manual(
        values = setNames(
          c("#F8766D", "#00BFC4"),
          c(
            translate_gender("M", input$lang),
            translate_gender("V", input$lang)
          )
        )
      ) +
      scale_y_continuous(labels = scales::label_percent(scale = 1)) +
      facet_wrap(~ tipo_P_trad) +
      labs(
        title = t("trendSessoDiagnTitle", input$lang),
        x     = NULL,                     # <— rimuove “year_month”
        y     = t("event_count", input$lang),
        color = t("sesso23",     input$lang)
      ) +
      theme_minimal() +
      theme(
        axis.text.x        = element_text(angle = 45, hjust = 1),
        legend.position    = "bottom",
        legend.direction   = "horizontal",
        legend.title.align = 0.5,
        legend.key.size    = grid::unit(0.5, "lines")
      )
    
    if (length(input$diagnosis) > 1) {
      p <- p +
        theme(
          panel.spacing.x  = grid::unit(0.1, "pt"),
          panel.border     = element_rect(
            colour = ggplot2::alpha("grey20", 0.4),
            size   = 0.5, fill = NA
          ),
          panel.background = element_rect(fill = "white", colour = NA)
        )
    }
    
    # 3) Trasforma in Plotly
    plt <- ggplotly(p,
                    tooltip      = "text",
                    dynamicTicks = TRUE
    )
    
    # 4) Configurazione comune per gli assi X
    axis_config <- list(
      type        = "date",
      tickmode    = "auto",
      nticks      = 6,
      tickangle   = 45,
      tickformat  = "%b %Y",
      hoverformat = "%d %b %Y"
    )
    
    # 5) Applica la configurazione: titolo solo sul primo asse, sul secondo lo leviamo
    plt %>%
      layout(
        hovermode   = "closest",
        margin      = list(b = 90),  # spazio sotto per la annotation
        # azzeri i titoli automatici su entrambi gli assi X
        xaxis       = c(axis_config, list(showlastlabel = FALSE, title = "")),
        xaxis2      = c(axis_config, list(matches = "x", title = "")),
        # unica annotation per il titolo X, centrata tra i due facet
        annotations = list(
          list(
            text      = t("year_month", input$lang),
            x         = 0.5,            # centro esatto del canvas
            xref      = "paper",
            xanchor   = "center",
            y         = -0.3,          # abbassa sotto i tick; regola se necessario
            yref      = "paper",
            showarrow = FALSE,
            font      = list(size = 14)
          )
        )
      ) %>%
      config(locale = input$lang)
    
  })
  
  
  ############################### TAB TEMPERATURA START ###############################
  output$sliderTemp <- renderUI({
    sliderInput("dateRangeTemp", label = t("periodoOsservazioneTemp", input$lang),
                min = as.Date(min(T_max_long$Fecha)),
                max = as.Date(max(T_max_long$Fecha)),
                value = c(as.Date(min(T_max_long$Fecha)), as.Date(max(T_max_long$Fecha))),
                timeFormat = "%Y-%m-%d")
  })
  
  output$selectLocalita2 <- renderUI({
    selectInput("localita", 
                label = t("selezionaLocalitaTitle", input$lang),
                choices = setNames(names(localita_codici_postali), sapply(names(localita_codici_postali), function(x) {
                  if(x == "Zona di Studio") {
                    return(t("zonaStudioTitle", input$lang))
                  } else {
                    return(x)
                  }
                })),
                selected = "Zona di Studio",
                multiple = TRUE)
  })
  output$selectcodpost2 <- renderUI({
    # estrai i codici “raw” e crei le etichette con lo zero davanti
    raw  <- unique(T_max_long$codpost) %>% as.character()
    disp <- stringr::str_pad(raw, width = 5, side = "left", pad = "0")
    
    selectInput(
      inputId  = "codpost",
      label    = t("SelectSpecCod", input$lang),
      choices  = setNames(raw, disp),  # values = raw, labels = padded
      selected = NULL,
      multiple = TRUE
    )
  })
  
  
  observeEvent(input$toggleCodpost2, {
    toggle("selectcodpost2_container", anim = TRUE)
    # aggiorna l'icona
    new_icon <- if (isTRUE(input$toggleCodpost2 %% 2 == 1)) "minus" else "plus"
    updateActionButton(session, "toggleCodpost2", icon = icon(new_icon))
  })# Reactive expression per i dati filtrati
  output$temperTabTitle <- renderText({
    # Per esempio, aggiungi una chiave "postalTabTitle" al dizionario
    t("trendTempTitle", input$lang)
  })
  
  # Filtra i dati meteo in base al periodo selezionato
  filtered_temp <- reactive({
    req(input$dateRangeTemp)
    T_max_long %>%
      filter(Fecha >= input$dateRangeTemp[1], Fecha <= input$dateRangeTemp[2])
  })
  # Dentro server, subito dopo gli altri renderUI che hai già…
  output$selectPlotTypeTemp <- renderUI({
    req(input$lang)
    label_trend <- switch(input$lang,
                          it = "Trend Temperatura",
                          en = "Temperature Trend",
                          es = "Tendencia de Temperatura",
                          ca = "Tendència de Temperatura")
    label_anom <- switch(input$lang,
                         it = "Deviazioni Settimanali",
                         en = "Weekly Deviations",
                         es = "Desviaciones Semanales",
                         ca = "Desviacions Setmanals"
    )
    selectInput(
      inputId  = "plotTypeTemp",
      label    = NULL,
      choices  = setNames(
        c("trend", "anomalie"),
        c(label_trend, label_anom)
      ),
      selected = "trend",
      width    = "100%"
    )
  })
  
  
  data_filtered_temp <- reactive({
    req(input$dateRangeTemp)
    
    df_meteo <- T_max_long %>%
      # 1) Trasformo Fecha e ottengo codpost_raw senza padding
      mutate(
        Fecha       = as.Date(Fecha),
        codpost_raw = as.character(codpost)
      ) %>%
      # 2) Calcolo la colonna localita basandomi su codpost_raw (senza 0 iniziale)
      mutate(
        localita = purrr::map_chr(
          codpost_raw,
          ~ {
            loc <- names(localita_codici_postali)[
              sapply(localita_codici_postali, function(v) .x %in% v)
            ]
            if (length(loc) == 0) NA_character_ else loc
          }
        )
      ) %>%
      # 3) Ora eseguo il padding su codpost_raw per ottenere codpost a 5 cifre
      mutate(
        codpost = stringr::str_pad(codpost_raw, width = 5, side = "left", pad = "0")
      ) %>%
      # 4) Applico il filtro temporale
      filter(
        Fecha >= input$dateRangeTemp[1],
        Fecha <= input$dateRangeTemp[2]
      )
    
    # 5) Se “Zona di Studio” è selezionata, la interpreto come “tutte le località”
    sel_loc <- input$localita
    if ("Zona di Studio" %in% sel_loc) {
      sel_loc <- character(0)
    }
    
    # 6) Applico in cascata il filtro su localita e codpost (operatore OR)
    if (length(sel_loc) > 0 && length(input$codpost) > 0) {
      df_meteo <- df_meteo %>%
        filter(localita %in% sel_loc | codpost %in% input$codpost)
    } else if (length(sel_loc) > 0) {
      df_meteo <- df_meteo %>%
        filter(localita %in% sel_loc)
    } else if (length(input$codpost) > 0) {
      df_meteo <- df_meteo %>%
        filter(codpost %in% input$codpost)
    }
    
    df_meteo
  })
  
  
  
  # ---- all’inizio del server ----
  smoothOn <- reactiveVal(FALSE)
  observeEvent(input$toggleSmooth, { smoothOn(!smoothOn()) })
  
  
  
  # ---- renderPlotly completo ----
  output$trendTemperatura <- renderPlotly({
    req(input$dateRangeTemp)
    
    # Prepara i dati (come prima)
    df <- T_max_long %>%
      mutate(
        Fecha    = as.Date(Fecha),
        codpost  = as.character(codpost),
        localita = map_chr(codpost, ~ {
          loc <- names(localita_codici_postali)[sapply(localita_codici_postali, function(v) .x %in% v)]
          if (length(loc)==0) NA_character_ else loc
        })
      ) %>%
      filter(Fecha >= input$dateRangeTemp[1], Fecha <= input$dateRangeTemp[2])
    
    # Aggrega località (tutte se nessuna selezionata)
    if (length(input$localita)==0) {
      local_means <- df %>%
        filter(!is.na(localita)) %>%
        group_by(Fecha) %>%
        summarise(meanT = mean(Tmax, na.rm=TRUE), .groups="drop") %>%
        mutate(localita = t("zonaStudioTitle", input$lang))
    } else {
      local_means <- df %>%
        filter(!is.na(localita), localita %in% input$localita) %>%
        group_by(Fecha, localita) %>%
        summarise(meanT = mean(Tmax, na.rm=TRUE), .groups="drop")
    }
    
    # Aggrega codici postali se selezionati
    codpost_means <- NULL
    if (!is.null(input$codpost) && length(input$codpost)>0) {
      codpost_means <- df %>%
        filter(codpost %in% input$codpost) %>%
        group_by(Fecha, codpost) %>%
        summarise(meanT = mean(Tmax, na.rm=TRUE), .groups="drop") %>%
        rename(localita = codpost)
    }
    
    # Crea il label da usare in hover
    mk_label <- function(x, y, name) {
      paste0(
        t("date", input$lang), ": ", x, "<br>",
        round(y,1), " °C<br>",
        name
      )
    }
    
    # Inizializza plotly
    p <- plot_ly()
    
    # subito dopo aver creato local_means e codpost_means:
    locs   <- unique(local_means$localita)
    pal_loc <- brewer.pal(max(3, length(locs)), "Pastel1")[seq_along(locs)]
    cps    <- unique(codpost_means$localita)
    pal_cp  <- brewer.pal(max(3, length(cps)), "Pastel1")[seq_along(cps)]
    
    # nel loop delle località:
    for(i in seq_along(locs)) {
      loc <- locs[i]; sub <- filter(local_means, localita==loc)
      p <- add_lines(p,
                     x          = sub$Fecha,
                     y          = sub$meanT,
                     name       = loc,
                     line       = list(color = pal_loc[i], dash = "solid"),
                     hoverinfo  = "text",
                     text       = mk_label(sub$Fecha, sub$meanT, loc),
                     legendgroup= loc,
                     showlegend = TRUE
      )
    }
    
    # nel loop dei codici postali:
    for(j in seq_along(cps)) {
      cp <- cps[j]
      sub2 <- filter(codpost_means, localita == cp)
      p <- add_lines(p,
                     x           = sub2$Fecha,
                     y           = sub2$meanT,
                     name        = cp,
                     line        = list(color = pal_cp[j+1], width = 0.5),  # solid line, thinner
                     hoverinfo   = "text",
                     text        = mk_label(sub2$Fecha, sub2$meanT, cp),
                     legendgroup = cp,
                     showlegend  = TRUE
      )
    }
    
    # 6) Smoothing aggregato su tutte le serie selezionate
    if (smoothOn()) {
      # aggrega per data
      agg <- local_means %>%
        group_by(Fecha) %>%
        summarise(aggMean = mean(meanT, na.rm=TRUE), .groups="drop") %>%
        arrange(Fecha)
      
      # rolling mean 90 giorni con zoo
      agg$smooth90 <- zoo::rollmean(agg$aggMean, k = 90, fill = NA, align = "center")
      
      # unica linea smoothed in rosso
      p <- add_lines(
        p,
        x          = agg$Fecha,
        y          = agg$smooth90,
        name       = paste0(t("zonaStudioTitle", input$lang), " (smoothed)"),
        line       = list(color = "red", dash = "solid", width = 2),
        hoverinfo  = "none",
        showlegend = TRUE
      )
    }
    
    # Layout finale
    p %>%
      layout(
        title     = t("trendTempTitle", input$lang),
        xaxis     = list(
          title      = t("date", input$lang),
          tickformat = "%b %Y",         # abbreviazione mese+anno
          tickangle  = 45
        ),
        yaxis     = list(title = t("max_temp", input$lang)),
        legend    = list(
          title = list(text = t("selectLocation22", input$lang))
        ),
        showlegend = TRUE,
        hovermode = "closest"
      ) %>%
      config(locale = input$lang)        # passa la lingua a Plotly
  })
  
  
  
  #### PLOT ANOMALIE
  output$anomTemperatura <- renderPlotly({
    # 1) Verifico che sia selezionato “anomalie” e che data_filtered_temp() abbia dati
    req(input$plotTypeTemp == "anomalie", data_filtered_temp())
    df0 <- data_filtered_temp()
    
    # 2) Se dopo i filtri non ci sono righe, restituisco un grafico vuoto
    if (nrow(df0) == 0) {
      return(plotly_empty(type = "scatter", mode = "lines"))
    }
    
    # 3) Calcolo anomalie settimanali su tutto il sottoinsieme filtrato (df0)
    df_anom <- df0 %>%
      mutate(
        mes       = lubridate::month(Fecha),
        settimana = lubridate::floor_date(Fecha, unit = "week", week_start = 1)
      ) %>%
      # (a) media mensile su df0 intero
      group_by(mes) %>%
      mutate(media_mes = mean(Tmax, na.rm = TRUE)) %>%
      # (b) media settimanale su df0 intero
      group_by(settimana) %>%
      mutate(media_semana = mean(Tmax, na.rm = TRUE)) %>%
      ungroup() %>%
      # (c) differenza su ogni riga e poi media per settimana
      mutate(dif = media_semana - media_mes) %>%
      group_by(settimana) %>%
      summarise(dif = mean(dif, na.rm = TRUE), .groups = "drop") %>%
      arrange(settimana) %>%
      mutate(
        dif_pos = ifelse(dif > 0, dif, 0),
        dif_neg = ifelse(dif < 0, dif, 0)
      )
    
    # 4) Applico smoothing su 'dif' se richiesto (rolling mean k=3)
    if (smoothOn()) {
      df_anom <- df_anom %>%
        mutate(dif_smooth = zoo::rollmean(dif, k = 3, fill = NA, align = "center"))
    }
    
    # 5) Traduzioni per le etichette
    pos_label    <- switch(input$lang,
                           it = "Positiva",  en = "Positive",
                           es = "Positivo",  ca = "Positiva")
    neg_label    <- switch(input$lang,
                           it = "Negativa",  en = "Negative",
                           es = "Negativo",  ca = "Negativa")
    smooth_label <- switch(input$lang,
                           it = "Smoothed",  en = "Smoothed",
                           es = "Suavizado", ca = "Smoothed")
    
    ym_label   <- switch(input$lang,
                         it = "Settimana",
                         en = "Week",
                         es = "Semana",
                         ca = "Setmana")
    diff_label <- switch(input$lang,
                         it = "Differenza",
                         en = "Difference",
                         es = "Diferencia",
                         ca = "Diferència")
    
    # 6) Costruisco il ggplot
    p_anom <- ggplot(df_anom, aes(x = settimana)) +
      # barre positive
      geom_col(
        aes(
          y    = dif_pos,
          fill = pos_label,
          text = paste0(ym_label, ": ", settimana, "<br>",
                        diff_label, ": ", round(dif, 2))
        ),
        show.legend = TRUE
      ) +
      # barre negative
      geom_col(
        aes(
          y    = dif_neg,
          fill = neg_label,
          text = paste0(ym_label, ": ", settimana, "<br>",
                        diff_label, ": ", round(dif, 2))
        ),
        show.legend = TRUE
      ) +
      # linea di base a y=0
      geom_hline(yintercept = 0, color = "black") +
      # linea smoothed (se attivo)
      {
        if (smoothOn()) {
          geom_line(
            data  = df_anom,
            aes(y = dif_smooth, color = smooth_label),
            size        = 0.5,
            alpha       = 0.8,
            show.legend = TRUE
          )
        }
      } +
      # palette fill per barre (usando un vettore nominato)
      scale_fill_manual(
        name   = diff_label,
        values = setNames(
          c("#f53311", "#36abeb"),
          c(pos_label, neg_label)
        )
      ) +
      # palette color per linea smoothed (vettore nominato)
      scale_color_manual(
        name   = "",
        values = setNames("#8f0303", smooth_label)
      ) +
      labs(
        x = ym_label,
        y = diff_label
      ) +
      theme_minimal()
    
    # 7) Converto in Plotly e imposto la legenda in basso
    ggplotly(p_anom, tooltip = "text") %>%
      layout(
        title = switch(input$lang,
                       it = "Deviazioni Settimanali",
                       en = "Weekly Deviations",
                       es = "Desviaciones Semanales",
                       ca = "Desviacions Setmanals"
                       
        ),
        legend = list(
          orientation = "h",
          x           = 0.5,
          xanchor     = "center",
          y           = -0.4
        ),
        margin = list(
          b = 70,   # spazio inferiore sufficiente per mostrare la legenda
          t = 40    # (eventualmente) spazio superiore per il titolo
        )      )
  })
  
  
  
  ############################### TAB CP-CODIGO POSTAL ###############################
  # ─────────────────────────────────────────────────────────────────────────────
  
  ###############################################################################
  #################### SERVER POSTAL PLOT##################
  
  
  output$postalNote <- renderUI({
    req(input$postal_cat)
    if (input$postal_cat == t("Generale", input$lang)) {
      h5(t("postalNote1", input$lang))
    } else if (input$postal_cat == "Cardiovascular") {
      h5(t("postalNote2", input$lang))
    } else if (input$postal_cat == "Diabetes") {
      h5(t("postalNote3", input$lang))
    } else if (input$postal_cat == "Ambos") {
      h5(t("postalNote4", input$lang))
    }
  })
  
  output$postalCategoryUI <- renderUI({
    lang <- input$lang
    
    categories <- list(
      Generale      = list(it = "Generale", en = "General", es = "General", ca = "General"),
      Cardiovascular = list(it = "Cardiovascolare", en = "Cardiovascular", es = "Cardiovascular", ca = "Cardiovascular"),
      Diabetes      = list(it = "Diabete",      en = "Diabetes",    es = "Diabetes",    ca = "Diabetes"),
      Ambos         = list(it = "Entrambi",     en = "Both",        es = "Ambos",       ca = "Ambdós")
    )
    
    translated_choices <- setNames(
      nm     = sapply(categories, function(cat) cat[[lang]]),
      object = names(categories)
    )
    
    selectInput("postal_cat", 
                label   = t("selectDiagnosis", lang), 
                choices = translated_choices,
                selected = t("Generale", input$lang))
  })
  
  output$postalPlot <- renderPlotly({
    req(input$postal_cat)
    
    # 1) Leggo e uniformo archivio “standard”
    postal_data_full <- postal_standard[[input$postal_cat]] %>%
      mutate(
        codpost = as.character(codpost),
        codpost = stringr::str_pad(codpost, width = 5, side = "left", pad = "0")
      ) %>%
      left_join(codpost_localita_df, by = "codpost")
    
    # 2) Applico il filtro (codici_utili) e creo etichetta
    postal_data <- postal_data_full %>%
      filter(codpost %in% codici_utili, n > 0) %>%
      mutate(etichetta = paste0(codpost, " – ", localita))
    
    # 3) Creo ggplot e passo a plotly
    p <- ggplot(postal_data, aes(
      x    = reorder(etichetta, -n),
      y    = n,
      fill = I(fill_color[1])
    )) +
      geom_bar(stat = "identity") +
      labs(
        title = paste0(
          t("patient_distribution_postal_code", input$lang), ": ",
          t(input$postal_cat, input$lang)
        ),
        x = t("postal_code", input$lang),
        y = t(postal_data_full$lbl_count[1], input$lang)
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 9)
      ) +
      scale_x_discrete(guide = guide_axis(n.dodge = 2))
    
    gg <- ggplotly(p, tooltip = c("x", "y"))
    for (i in seq_along(gg$x$data)) {
      gg$x$data[[i]]$hovertemplate <- paste0(
        t("postal_code", input$lang), ": %{x}<br>",
        t(postal_data_full$lbl_count[1], input$lang), ": %{y:.0f}<extra></extra>"
      )
    }
    gg
  })
  
  ###############################################################################
  ##################### SERVER MIRROR PLOT #####################################
  
  # 6.1) Calcolo pazienti e percentuali per località (reactive)
  patient_by_loc <- reactive({
    req(input$postal_cat)
    
    df0 <- postal_mirror[[ input$postal_cat ]] %>%
      mutate(
        codpost = stringr::str_pad(as.character(codpost), width = 5, side = "left", pad = "0")
      ) %>%
      { if (!is.null(codici_utili2)) filter(., codpost %in% codici_utili2) else . } %>%
      left_join(codpost_localita_df, by = "codpost") %>%
      filter(!is.na(localita)) %>%
      group_by(localita) %>%
      summarise(patients = sum(n, na.rm = TRUE), .groups = "drop")
    
    missing_locs <- setdiff(pop_by_loc$locality, df0$localita)
    if (length(missing_locs) > 0) {
      df0 <- bind_rows(
        df0,
        tibble(localita = missing_locs, patients = 0L)
      )
    }
    
    df0 %>%
      left_join(pop_by_loc, by = c("localita" = "locality")) %>%
      mutate(
        percent      = ifelse(total_pop > 0, patients / total_pop, 0),
        non_patients = pmax(total_pop - patients, 0),  
        percent_non  = ifelse(total_pop > 0, non_patients / total_pop, 0)
      ) %>%
      select(localita, total_pop, patients, non_patients, percent, percent_non)
  })
  
  # 6.2) Preparo i dati normalizzati per il grafico
  df_mirror <- reactive({
    req(patient_by_loc())
    combined <- patient_by_loc()
    
    ord_loc <- combined %>% arrange(desc(patients)) %>% pull(localita)
    
    combined <- combined %>%
      mutate(
        tot_raw    = total_pop,
        frac_pat   = ifelse(tot_raw > 0, patients / tot_raw, 0.5),
        frac_non   = ifelse(tot_raw > 0, non_patients / tot_raw, 0.5),
        norm_pat   = frac_pat * 25,
        norm_non   = frac_non * 25
      )
    
    df_pat <- combined %>%
      select(localita, patients, percent, norm_pat) %>%
      mutate(
        category    = "Pazienti",
        value       = norm_pat,
        localita    = factor(localita, levels = ord_loc),
        percent_pat = percent
      )
    
    df_non <- combined %>%
      select(localita, non_patients, norm_non, percent_non) %>%
      mutate(
        category     = "Popolazione non‐Pazienti",
        value        = norm_non,
        localita     = factor(localita, levels = ord_loc),
        percent_nonp = percent_non
      )
    
    perc_vals <- combined %>%
      mutate(localita = factor(localita, levels = ord_loc)) %>%
      arrange(localita) %>%
      pull(percent)
    
    list(
      df_pat    = df_pat,
      df_non    = df_non,
      ord_loc   = ord_loc,
      perc_vals = perc_vals
    )
  })
  
  # 6.3) Render della tabella (senza la colonna non_patients)
  output$mirrorTable <- DT::renderDataTable({
    df <- patient_by_loc() %>%
      arrange(desc(patients)) %>%
      select(localita, total_pop, patients, percent) %>%
      mutate(
        total_pop = formatC(total_pop, format = "d", big.mark = ".", decimal.mark = ","),
        patients  = formatC(patients,  format = "d", big.mark = ".", decimal.mark = ","),
        percent   = scales::percent(percent, accuracy = 0.1)
      ) %>%
      rename(
        # Qui usi la funzione t() per tradurre i nomi delle colonne
        !!t("locality",       input$lang) := localita,
        !!t("population_total", input$lang) := total_pop,
        !!t("n_patients",     input$lang) := patients,
        !!t("percent_column", input$lang) := percent
      )
    
    DT::datatable(
      df,
      rownames = FALSE,
      options = list(
        pageLength = 8,
        dom        = 'tp',
        columnDefs = list(list(className = "dt-center", targets = 1:3))
      ),
      class  = 'compact stripe hover',
      escape = FALSE
    ) %>%
      formatStyle(
        # Anche qui devi passare il vettore dei nomi tradotti
        columns = c(
          t("locality",       input$lang),
          t("population_total", input$lang),
          t("n_patients",     input$lang),
          t("percent_column", input$lang)
        ),
        fontSize = '10px'
      )
  })
  
  
  # 6.4) Render del grafico Plotly (testo interno = conteggi; hover = %)
  output$mirrorBarPlot <- renderPlotly({
    m         <- df_mirror()
    df_pat    <- m$df_pat
    df_non    <- m$df_non
    ord_loc   <- m$ord_loc
    perc_vals <- m$perc_vals
    
    plot_ly(orientation = "h") %>%
      # Segmento “Pazienti”
      add_trace(
        data = df_pat,
        x    = ~value,
        y    = ~localita,
        name = t("patients", input$lang),  # invece di "Pazienti" o "Patients"
        type = "bar",
        width = 1,           # qui imposti lo spessore (40% dello “step” disponibile)
        marker = list(
          color = "#d47264",
          line  = list(color = "rgba(0,0,0,0.3)", width = 0.5)
        ),
        hovertemplate = paste0(
          t("locality",    input$lang), ": %{y}<br>",
          t("percent_patients", input$lang), ": %{customdata:.1%}<extra></extra>"
        ),
        customdata = ~percent_pat,
        text       = ~patients,
        textposition     = "inside",
        insidetextanchor = "middle",
        textfont = list(color = "white", size = 9)
      ) %>%
      # Segmento “Popolazione non‐Pazienti”
      add_trace(
        data = df_non,
        x    = ~value,
        y    = ~localita,
        name = t("non_patients", input$lang),  # invece di "Popolazione non‐Pazienti"
        type = "bar",
        width = 1,           # qui imposti lo spessore (40% dello “step” disponibile)
        marker = list(
          color = "#4dc0c4",
          line  = list(color = "rgba(0,0,0,0.3)", width = 0.5)
        ),
        hovertemplate = paste0(
          t("locality",       input$lang), ": %{y}<br>",
          t("percent_non_patients", input$lang), ": %{customdata:.1%}<extra></extra>"
        ),
        customdata = ~percent_nonp,
        text       = ~non_patients,
        textposition     = "inside",
        insidetextanchor = "middle",
        textfont = list(color = "white", size = 9)
      ) %>%
      layout(
        barmode = "stack",
        bargap  = 0,
        xaxis = list(
          title          = t("normalized_scale", input$lang),   # definisci "normalized_scale" nelle tue chiavi
          autorange      = "reversed",
          showticklabels = FALSE,
          zeroline       = FALSE
        ),
        yaxis = list(
          title = list(
            text = t("y_axis_title", input$lang),
            font = list(size = 9)
          ),
          tickfont = list(size = 9),
          automargin    = TRUE,
          categoryorder = "array",
          categoryarray = ord_loc
        ),
        yaxis2 = list(
          overlaying    = "y",
          side          = "right",
          title = list(
            text = t("percent_patients", input$lang),  # “% Pazienti” → chiave "percent_patients"
            font = list(size = 14)
          ),
          tickmode      = "array",
          tickvals      = seq(0, length(ord_loc) - 1),
          ticktext      = paste0(round(perc_vals * 100, 1), "%"),
          tickfont      = list(size = 10),  # dimensione del font dei tick su yaxis2
          showgrid      = FALSE,
          showline      = TRUE
        ),
        
        legend = list(
          orientation = "h",
          x           = 0.5,
          xanchor     = "center",
          y           = -0.35
        ),
        margin = list(
          l = 0,
          r = 25,
          t = 10,
          b = 10
        )
      ) %>%
      config(displayModeBar = FALSE)
  })
  ############################### TAB EDAD ###############################
  output$selectEdadMode <- renderUI({
    # Imposta le opzioni: includi "Generale" e tutte le possibili diagnosi
    # Applichiamo la traduzione per ciascuna opzione
    translated_choices <- c(t("Generale", input$lang),t("diagnTabTitle", input$lang))
    general_input <- c(t("Generale", input$lang))
    selectInput("tipo_P", 
                label = t("selectDiagnosis", input$lang),  # Assicurati di avere una traduzione per questo label
                choices = translated_choices,
                selected = general_input,  
                multiple = FALSE)
  })
  
  
  output$graficoEta <- renderPlot({
    req(input$tipo_P)
    if (input$tipo_P == t("Generale", input$lang)) {
      media_eta_g <- mean(demog_age$age, na.rm = TRUE)
      
      ggplot(demog_age, aes(x = age)) +
        geom_histogram(binwidth = 5, fill = "skyblue", color = "white") +
        geom_vline(xintercept = media_eta_g, linetype = "dashed", color = "red") +
        annotate(
          "text",
          x     = media_eta_g,
          y     = 0,          # fissa l’etichetta all’altezza 0 (base dell’istogramma)
          label = paste0(
            t_mean_age("label", input$lang), ": ",
            floor(media_eta_g + 0.5), " ",
            t_mean_age("suffix", input$lang)
          ),
          vjust = 1,          # “su” il valore y=0
          hjust = 0.5,
          fontface = "bold"
        )+
        labs(
          title = t("etaMediaTitle", input$lang),
          x     = t("xAge", input$lang),
          y     = t("yFrequency", input$lang)
        ) +
        theme_minimal() +
        theme(
          legend.position    = "bottom",
          legend.direction   = "horizontal",
          legend.title.align = 0.5,
          legend.key.size    = unit(0.5, "lines")
        )
    }
    else {
      # === branch “Diagnosi”: prendi TUTTI i tipi e plotta tutto insieme ===
      
      # prendi tutti i tipi distinti dal data frame completo
      all_types <- sort(unique(data_age_full$tipo_P))
      
      # filtra tutto
      df <- data_age_full %>%
        filter(tipo_P %in% all_types)
      
      # calcola le medie per ogni tipo
      medie_eta <- df %>%
        group_by(tipo_P) %>%
        summarise(media_eta = mean(age, na.rm = TRUE), .groups = "drop") %>%
        mutate(tipo_P_trad = translate_tipo_P(tipo_P, input$lang))
      
      # trasforma per ggplot
      df$tipo_P_trad <- factor(
        df$tipo_P,
        levels = medie_eta$tipo_P,
        labels = medie_eta$tipo_P_trad
      )
      
      # disegna l’istogramma a barre affiancate + linee medie + facet
      ggplot(na.omit(df), aes(x = age, fill = tipo_P_trad)) +
        geom_histogram(binwidth = 5, color = "white", position = "dodge") +
        geom_vline(
          data = medie_eta,
          aes(xintercept = media_eta),
          linetype = "dashed", color = "red"
        ) +
        geom_text(
          data = medie_eta,
          aes(
            x     = media_eta, y = 0,
            label = paste0(
              t_mean_age("label", input$lang), ": ",
              floor(media_eta + 0.5), " ",
              t_mean_age("suffix", input$lang)
            )
          ),
          vjust = -1, hjust = 0.5, fontface = "bold"
        ) +
        labs(
          title = t("distEtaDiagTitle", input$lang),
          x     = t("xAge",      input$lang),
          y     = t("yFrequency", input$lang)
        ) +
        scale_fill_discrete(name = t("tipoPaziente", input$lang)) +
        facet_wrap(~ tipo_P_trad, scales = "free_y") +
        theme_minimal() +
        theme(
          legend.position    = "bottom",
          legend.direction   = "horizontal",
          legend.title.align = 0.5,
          legend.key.size    = unit(0.5, "lines")
        )
    }
  })
  
}
# ========================================================================================================================================================
#                                                                      RUN
# ========================================================================================================================================================

# Run the application 
shinyApp(ui = ui, server = server)
