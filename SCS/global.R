
demo_P = data.frame(
  t = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0),
  P = c(0.02, 0.04, 0.10, 0.14, 0.16, 0.16, 0.14, 0.10, 0.04, 0.02)
)

demo_soilclasses = data.frame(
  area = c(10, 40, 30, 20)
)
row.names(demo_soilclasses) = c("A", "B", "C", "D")

demo_landuse = c(
  "Wohngebiete (Mittl. Grundstücksgröße bis 1000m^2, Versiegelungsgrad: 38%)",
  "Straßen (asphaltiert mit Randstein und Abwasserkanalisation)",
  "Wald (kein Mulch, gute Bodenbedeckung)",
  "Wiese (in gutem Zustand)",
  "Weideland (in schlechtem Zustand)",
  "Landwirtschaftlich genutzte Ackerfläche (ohne Erosionsschutzmaßnahmen)"
)

demo_area = c(6, 4, 12, 7, 6, 8)

scs_classification = data.frame(
  use = c(
    "Landwirtschaftlich genutzte Ackerfläche (mit Erosionsschutzmaßnahmen)",
    "Landwirtschaftlich genutzte Ackerfläche (ohne Erosionsschutzmaßnahmen)",
    "Weideland (in schlechtem Zustand)",
    "Weideland (in gutem Zustand)",
    "Wiese (in gutem Zustand)",
    "Wald (lichter Bestand, schlechte Bodendeckung)",
    "Wald (kein Mulch, gute Bodenbedeckung)",
    "Rasen, Parks, Golfplätze, Friedhöfe etc. (guter Zustand, Grasbewuchs auf 75 % der Fläche)",
    "Rasen, Parks, Golfplätze, Friedhöfe etc. (mäßiger Zustand, Grasbw. auf 50–75% der Fläche)",
    "Städt. Wohn- und Geschäftsviertel (85% Versiegelungsgrad)",
    "Industriegebiete (72% Versiegelungsgrad)",
    "Wohngebiete (Mittl. Grundstücksgröße bis 500m^2, Versiegelungsgrad: 65%)",
    "Wohngebiete (Mittl. Grundstücksgröße bis 1000m^2, Versiegelungsgrad: 38%)",
    "Wohngebiete (Mittl. Grundstücksgröße bis 1500m^2, Versiegelungsgrad: 30%)",
    "Wohngebiete (Mittl. Grundstücksgröße bis 2000m^2, Versiegelungsgrad: 25%)",
    "Wohngebiete (Mittl. Grundstücksgröße bis 4000m^2, Versiegelungsgrad: 20%)",
    "Asphaltierte Flächen, Parkplätze, Einfahrten, Dächer",
    "Straßen (asphaltiert mit Randstein und Abwasserkanalisation)",
    "Straßen (Schotterstraßen)",
    "Straßen (Erdstraßen)"
  ),
  A = c(72, 62, 68, 39, 30, 45, 25, 39, 49, 89, 81, 77, 61, 57, 54, 51, 98, 98, 76, 72),
  B = c(81, 71, 79, 61, 58, 66, 55, 61, 69, 92, 88, 85, 75, 72, 70, 68, 98, 98, 85, 82),
  C = c(88, 78, 86, 74, 71, 77, 70, 74, 79, 94, 91, 90, 83, 81, 80, 79, 98, 98, 89, 87),
  D = c(91, 81, 89, 80, 78, 83, 77, 80, 84, 95, 93, 92, 87, 86, 85, 84, 98, 98, 91, 89)
)


source("functions.R")
