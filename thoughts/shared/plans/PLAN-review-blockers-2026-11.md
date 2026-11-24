# Behebungsplan: die bestätigten Blocker des Code-Reviews

Grundlage: der Gesamt-Review vom 21.–23.11.2026 (735 Rohbefunde, davon 110 mit Schwere blocker/major gegengelesen; die IDs wie `s1-app-28` verweisen auf dessen Befunddateien, die nicht im Repository liegen). Zeilennummern stammen aus dem geprüften Stand (`dev` @ `ec76a8f5` + File-drop-Fix); `dev` hat sich seither bewegt (File-drop-Fix in PR #226, Release 0.29.0), die Stellen sind über die Symbolnamen wiederzufinden.

Es gelten die Projektregeln: Arbeit geht über Pull Requests gegen `dev`, kein Versions-Bump, `CHANGELOG.md` nur unter `

## 0. Umfang

Acht bestätigte Befunde in fünf voneinander unabhängigen Themen, daraus fünf PRs:

| PR | Themen | Befunde | Art |
| --- | --- | --- | --- |
| A | Status-Map meldet "Passed" über ungeprüften Tabellen | `s1-app-28`, `s2-shiny-r-03` | App-Fehler |
| B | Regel-Editor: verlustfreier Round-Trip | `s1-app-01`, `s1-app-02`, `s1-app-03` | App-Fehler |
| C | Leerer Vokabular-Slot ("None") | `t-shiny-1-14` (+ `s2-shiny-r-20`, `-21`) | App-Fehler, braucht Entscheidung D1 |
| D | `pattern` und leerer String (REQ-RULE-007), plus `pattern` auf numerischen Spalten (gleiche Zeile) | `q3-qual-tests-b-27` (+ `c2-data-25`, major) | Produktfehler plus Test, braucht Entscheidung D2 |
| E | OQ-ROBUST-012 ist eine Tautologie | `q3-qual-tests-b-17` | nur Test, kein Produktänderung |

Nicht im Umfang (eigene Pläne später): die als major bestätigten Befunde, darunter die Traceability-Regel der Qualifikation. Der Befund `t-shiny-2-10` (der `suspendWhenHidden`-Test) ist noch offen und wird in Schritt 0 geklärt.

## 1a. Entschieden am 24.11.2026 (alle vier Empfehlungen angenommen)

- **D1:** "None" entfernt die Vokabular-Einschränkung der Spalte (`values` und `values_from` werden `NULL`); `mode: open` mit nur eigenen Begriffen ergibt genau diese Begriffe; ein Slot mit `min > 0` darf nicht leer bestätigt werden, sofern der Dialog das nicht schon durchsetzt.
- **D2:** Die Software wird angepasst: leerer String und `NA` matchen nie auf ein `pattern`; der `pattern`-auf-Zahlen-Fehler (`c2-data-25`) wird im selben PR D behoben.
- **D3:** Der Editor schreibt Operator-Aliase in die Normalform (`range` ⇒ `min`/`max`, `equal` ⇒ `equals`, `not_equal` ⇒ `not_equals`); im CHANGELOG vermerken.
- **D4:** Nacheinander in der Reihenfolge A → C → D → E → B, höchstens zwei Sessions gleichzeitig.

**Freigabe:** Die Umsetzung wurde am 24.11.2026 freigegeben; sie beginnt mit PR A.

## 1. Entscheidungen, die du treffen musstest (Ausgangslage)

**D1 (PR C): Was bedeutet "None" bei einem Vokabular-Slot?** Im Code ist es heute nicht definiert. Der Dialog zeigt "0 von 2 ausgewählt", der Kommentar in `app.R` verspricht "no terms", gebaut wird das komplette Vokabular.
- *Empfehlung (a):* "None" entfernt die Vokabular-Einschränkung der Spalte: `values` und `values_from` werden gelöscht, die Spalte ist unbeschränkt. Das ist das einzige Ergebnis, das ein gültiges Spezifikationsobjekt ergibt (ein leeres `values` wäre entweder "keine Einschränkung" oder "nichts erlaubt", und `as_json_schema()` zerbricht an `character(0)`, siehe `t-core-2-36`).
- (b): "None" ist nur erlaubt, wenn der Slot `min: 0` hat, sonst verweigert der Dialog die Bestätigung. Ergänzung zu (a), nicht Alternative.
- In `mode: open` bedeutet eine Auswahl nur aus eigenen Begriffen genau diese Begriffe, nie "alle plus eigene" (`s2-shiny-r-20`). Das ist unstrittig.

**D2 (PR D): Soll ein leerer String auf ein `pattern` passen?** REQ-RULE-007 (Risiko hoch) sagt "nein", die Software sagt ja, und keine Abweichung ist registriert.
- *Empfehlung (a):* Operator anpassen, sodass `""` und `NA` nie matchen. Das ist das, was die Anforderung verspricht, und es macht `""` gleich zu `NA`, das schon heute nie matcht. Wer leere Werte prüfen will, hat `empty`. Der Praxis-Effekt ist klein: `load_file()` macht leere Textfelder schon vor den Regeln zu `NA` (REQ-FILE-004). Betroffen sind Aufrufer, die `rule_check_col_condition()` direkt mit Data Frames füttern.
- (b): REQ-RULE-007 umschreiben ("ein leerer String wird wie jeder andere String gematcht") und das Verhalten per Test festhalten. Das verlangt, den Satz in Guide und roxygen mitzuändern.

**D3 (PR B): Operatoren-Aliase.** `range: [lo, hi]` wird im Editor zu `min`/`max`, `equal`/`not_equal` zu `equals`/`not_equals`. Semantik bleibt gleich, nur die Schreibweise im gespeicherten YAML ändert sich beim ersten Speichern. *Empfehlung:* so umsetzen und im CHANGELOG nennen. Die Alternative, die Schreibweise zu bewahren, kostet einen versteckten Originalzustand je Zeile ohne Nutzen für den Anwender.

**D4: Reihenfolge und Parallelität.** *Empfehlung:* **nacheinander** in der Merge-Reihenfolge A → C → D → E → B, höchstens zwei gleichzeitig (etwa D und E, die sich nicht überschneiden). Gründe: Das 5-Stunden-Limit hat das Review fünfmal abgebrochen, und jede Bearbeitung braucht eigene Test- und Review-Läufe. Außerdem berühren A, B und C alle `app.R`, A, B und C `REQ-APP.yaml`, alle fünf `CHANGELOG.md` und vier `inst-files.sha256` (Rebase-Konflikte). Nach jedem Merge Manifest neu synchronisieren (Abschnitt 8).

## 2. Schritt 0: zwei offene Fragen klären (vor den PRs, je etwa 15 Minuten)

1. `t-shiny-2-10`: In einem minimalen `testServer()` `outputOptions()` aufrufen und den Rückgabewert ausgeben. Ergebnis `NULL` und keine Wirkung auf die Optionen ⇒ der Test `test-shinyapp-dataset-add-remove.R` (Zeilen um 283) ist wirkungslos und wird als major in einen eigenen Plan aufgenommen. Das ist ein `Rscript` im Worktree mit `.Renviron`-Trick (siehe die lokale Umgebungsnotiz zum Worktree).
2. S7: aktuelles CRAN-S7 in eine saubere Testbibliothek installieren, `DTARuleColRange(id = "r", columns = "A", range = c(1, 2))` aufrufen, `args(S7::new_object)` ansehen. Für diese PRs nicht blockierend, aber es entscheidet, ob eine eigene Reparatur dringend ist. *Benötigt dein Okay, weil es ein Paket von CRAN lädt.*

## 3. PR A: Status-Map (`s1-app-28`, `s2-shiny-r-03`)

**Ursache.** `dta_status_map()` (`inst/shiny/dta_app/R/utils_dta.R:1253-1278`) klassifiziert "pass", sobald `n_validated > 0 || n_valid > 0` und `n_invalid == 0`. `n_targets` und `n_not_validated` liest es nie. `results()` wiederholt diese Zähler auf jeder Zeile des Datasets (`R/validationReporting.R:86-91`), die Map *summiert* sie über die Zeilen (`sum(n_valid[idx])`), zählt also doppelt. `run_check()` (`app.R:4697-4705`) überschreibt zusätzlich die **gesamte** Map nach dem Prüfen einer Teilmenge, sodass ein vom App-Code selbst auf "pending" gesetztes Dataset zu "pass" wird.

**Änderung.**
1. `dta_status_map()`: Zähler je Dataset **einmal** lesen (erste Zeile, nicht summieren). Regel: `fail`, wenn `n_invalid > 0`; `pass` nur, wenn `n_targets > 0`, `n_not_validated == 0`, `n_invalid == 0` und `n_validated > 0`; sonst `pending`. Was `skipped` bedeutet, vor dem Codieren in `validation_status()` nachlesen (R/DTADataSetTabular-class.R:1197-1224) und festlegen; Standard: übersprungene Tabellen verhindern "pass".
2. `run_check()`: **zuerst nur Änderung 1 umsetzen und testen.** Liest `dta_status_map()` `n_not_validated` korrekt, ist die globale Map selbst richtig, und das Zurückschreiben der ganzen Map ist dann womöglich unschädlich. Nur wenn die Tests unten (Upload, Überschreiben, Lieferung) damit noch "pass" statt "pending" zeigen, die Einträge auf die tatsächlich geprüften Datasets (`ready`) beschränken. Die Einschränkung hat ein eigenes Risiko: Prüft man ein Dataset, kann dessen Ergebnis den Status eines anderen berühren (z. B. über Dateizahl-Befunde), und eine auf `ready` beschränkte Übernahme würde den alten Wert dort stehen lassen. Wird sie nötig, bleibt der Dateizahl-Befund aus `dta_status_map()` für alle Datasets erhalten.
3. Berichtsbanner (`utils_dta.R:2452-2548`): liest die Map und stimmt danach von selbst; per Test absichern, nicht anfassen.

**Tests (zuerst rot).**
- `test-shinyapp-dta-ops.R`: Fixture mit Dataset aus zwei Tabellen, eine validiert und gültig, die zweite neu gebunden und nie geprüft ⇒ erwartet `"pending"`. Das Orakel ist von Hand gebaut, nicht aus `results()` abgeleitet. Dazu: zwei gültige Tabellen ⇒ `"pass"`; eine ungültige, eine ungeprüfte ⇒ `"fail"`; eine Tabelle mit `skipped`.
- Server-Test (`testServer` mit der echten App), dreimal, je Weg, über den eine ungeprüfte Tabelle in ein Dataset kommt: **Upload** (`lab_2.csv` neu, `app.R:3514-3516`), **Überschreiben** (gleicher Dateiname ersetzt, `3485-3491`) und **Lieferung** (`3825-3827`). Jeweils: Dataset `lab` war bestanden, danach `run_check()` für `dm` ⇒ `rv$status[["lab"]]` bleibt `"pending"`, und die Zusammenfassung wird nicht angeboten. Zeigt der Test mit Änderung 1 allein "pending", entfällt Änderung 2.
- Report-Test: `dta_build_validation_report()` mit dieser Konstellation enthält nicht "VALIDATION PASSED".
- Mutationsprobe: Fix zurücknehmen ⇒ alle drei roten Tests werden rot (vor dem Merge protokollieren).

**Qualifikation.** `REQ-APP` kennt die vier Status-Werte (Zeilen um 129-138) und die Prüfung, dass nach einem Edit gecheckt wird (um 384-395). Neue Anforderung: "ein Dataset gilt nur als `pass`, wenn jede gebundene Tabelle validiert wurde und keine fehlgeschlagen ist" mit einem OQ-APP-Fall, dessen Erwartung aus dem Anforderungstext folgt (zwei Tabellen, eine ungeprüft ⇒ nicht `pass`).

## 4. PR B: Regel-Editor (`s1-app-01`, `-02`, `-03`)

**Ursache.** Das Formular modelliert eine Bedingung als "eine Zeile je Spalte, ein Operator, Wert als Text". Offen und Speichern ohne Änderung ist deshalb nicht die Identität:
- Operator außerhalb der UI-Liste (`range`, `equal`, `not_equal`) steht im `selectInput` nicht zur Wahl, der Browser meldet die erste Option `equals`. `not_equal` wird zum Gegenteil, `range: [0.5, 40]` zu `equals: "0.5, 40"` (`cond_to_rows`, `app.R:6399-6427`).
- Mehrere Operatoren je Spalte (`{greater_equal: 18, less: 65}`, vom Guide vorgeschrieben): nur der erste wird angezeigt, beim Speichern überschreibt `out[[col]] <- inner` (6479). Das Gruppen-Pendant `collect_group_conditions()` (6601) merged dagegen richtig.
- `parse_cond_value()` (6428-6457) wandelt jeden Wert mit `as.numeric()` um: `"001"` wird `1`, `"007"` wird `7`, `"1.50"` wird `1.5`; `in`-Listen werden mit `", "` verbunden und an `,` zerlegt, `"Smith, John"` wird zu zwei Werten. Bei einer Char-Spalte vergleicht die Engine `"001" %in% c(1, 2)` als FALSE: die IF-Klausel wählt keine Zeile mehr, die Regel kann nie mehr fehlschlagen.

**Entwurf (Empfehlung).** Leitprinzip: *Öffnen und Speichern ohne Änderung ist semantisch die Identität.*
1. **Reine Funktionen herausziehen.** `cond_to_rows`, `parse_cond_value` und `collect_cond` sind heute Closures innerhalb von `server()` und deshalb nur über `testServer` erreichbar. Neue Datei `inst/shiny/dta_app/R/rule_conditions.R` mit `condition_to_form_rows()` und `form_rows_to_condition()` (und dem Wert-Parser). `app.R` ruft diese auf. Keine Verhaltensänderung bei dieser Herauslösung, sie ist nur der Grund, warum die Funktionen überhaupt direkt testbar werden. Prüfen, wie `app.R` die Dateien in `R/` einbindet (sourcing-Schleife), und die Datei dort eintragen.
2. **Eine Zeile je (Spalte, Operator).** `min` und `max` bleiben eine Zeile `min_max`; `range` wird ebenfalls zu `min_max`; `equal`/`not_equal` werden zu `equals`/`not_equals` (D3); jeder weitere Schlüssel bekommt eine eigene Zeile.
3. **Speichern führt Zeilen derselben Spalte zusammen** wie `collect_group_conditions()`. Zweimal derselben Operator in derselben Spalte ⇒ sichtbare Fehlermeldung, kein stilles Überschreiben.
4. **Wert-Typen.**
   - Der Wert einer unveränderten Zeile bleibt wortgleich der ursprüngliche R-Wert (Zustand je Zeile im Formular, `rv$cond_orig`).
   - Eine geänderte Zeile wird nach dem deklarierten Typ der Spalte geparst: numerische Spalte ⇒ Zahl, Textspalte ⇒ Text, nie nach "sieht aus wie eine Zahl". **Vorab prüfen:** ob dem Formular die deklarierten Typen zur Verfügung stehen. `cols` ist vermutlich nur die Liste der Spaltennamen; die Typen kommen aus den Spaltenspezifikationen des Datasets (`DTAColumnSpec`-Struktur) und müssen eigens an die Parser-Funktion durchgereicht werden. Ist die Spalte unbekannt oder untypisiert, gilt Text.
   - `in`/`not_in`: Anzeige und Parser mit Anführungszeichen-Regel (`"Smith, John", Doe`), eingelesen mit einem echten CSV-Zeilenparser (`utils::read.csv(text=, header=FALSE)` oder `scan(sep=",", quote='"')`) statt `strsplit`.
5. **Kein stilles Verwerfen.** `parse_cond_value` verwirft heute unlesbare Zahlen still (`suppressWarnings(as.numeric())`, siehe `s1-app-04`, major, gleiche Funktionen). Empfehlung: im selben PR beheben (Fehlermeldung statt `NA`), weil es dieselben Zeilen sind.
6. **Operator-Drift verhindern.** Die App hat eine **zweite** `dta_condition_operators()` (`utils_dta.R:2936`), die die gleichnamige Funktion des Pakets (`R/evaluateRules.R:875`) überdeckt. Ein Test vergleicht beide: jedes Engine-Element (`equals, equal, not_equals, not_equal, in, not_in, greater, less, greater_equal, less_equal, min, max, range, pattern, empty`) muss im Formular darstellbar sein (Menge oder Alias). Zugriff auf das interne Paket-Objekt im Test über `getFromNamespace()`.

**Tests (zuerst rot), alle auf den reinen Funktionen.**
- Round-Trip-Korpus aus dem Guide und den Findings: `LBORRES: {range: [0.5, 40]}`, `VISIT: {not_equal: SCREENING}`, `AGE: {greater_equal: 18, less: 65}`, `SITEID: {in: ["001","002"]}`, `equals: "007"`, `equals: "1.50"`, `in: ["Smith, John","Doe"]`, `empty: false`, `pattern: "^A"`, `min` allein, `max` allein.
- **Primäres Orakel, von Hand geschrieben:** für jeden Korpuseintrag steht die erwartete Liste nach Öffnen und Speichern **wörtlich** im Test (Aliase von dir aufgelöst: `range: [0.5, 40]` ⇒ `list(min = 0.5, max = 40)`, `not_equal: SCREENING` ⇒ `list(not_equals = "SCREENING")`, `"007"` bleibt `"007"`). Kein Normalisierer im Test, weil dieser sonst selbst der geprüfte Code wäre und der Test sich mit sich selbst verglichen hätte (das Muster, das das Review bei `q3-56`/`q3-59` bemängelt hat).
- **Sekundär (metamorphisch):** `dta_condition_mask()` auf einem von Hand gebauten Data Frame vor und nach dem Round-Trip liefert dieselbe Maske. Das fängt Fehler im Beispiel-Korpus, ersetzt aber das wörtliche Orakel nicht.
- **Invariante gegen den Browser-Rückfall:** jeder Operator, den `condition_to_form_rows()` ausgibt, ist ein Wert von `dta_condition_operators()` der App, also eine wählbare Option des `selectInput`. Damit ist der Rückfall "keine passende Option ⇒ erste Option" unmöglich. Der Test wirkt auf der reinen Funktion; ein `testServer`-Test könnte den Fehler nicht zeigen, weil der Fehler im Browser entsteht (dort meldet ein `<select>` ohne passende Option die erste Auswahl) und `input$cond_op_i` im `testServer` der Test selbst setzt.
- Speichern zweier Zeilen derselben Spalte ⇒ Zusammenführung; zweimal derselbe Operator ⇒ Fehler.
- Drift-Test gegen die Engine-Operatoren (siehe Entwurf Punkt 6).

**Qualifikation.** Neue Anforderung in `REQ-APP`: "Eine Regel, die im Editor geöffnet und ohne Änderung gespeichert wird, behält ihre Semantik." OQ-APP-Fall mit den Guide-Beispielen (Erwartung aus dem Guide, nicht aus der App).

**Laufzeitbeweis im Browser.** Das Verhalten eines `<select>` ohne passende Option ist Browser-Verhalten und mit `testServer` nicht zu belegen. Deshalb: vor und nach dem Fix je einmal im echten Browser (das Skript für headless Edge aus dem Projekt-Memory, weil der Browser-Pane nicht verlässlich rendert) den Guide-Fall öffnen und ohne Änderung speichern: `LBORRES: {range: [0.5, 40]}` darf nicht zu `equals: "0.5, 40"` werden. Das Ergebnis kommt in die PR-Beschreibung.

## 5. PR C: Leerer Vokabular-Slot (`t-shiny-1-14`, `s2-shiny-r-20`, `-21`)

**Ursache.** `vocabulary_slot_values()` (`inst/shiny/dta_app/R/vocabulary.R:788-811`; der Pfad liegt unter `inst/shiny/dta_app/`, nicht im Paket-`R/`) gibt für "bewusst geleert" (`character(0)` aus `collect_vocab_selections()`, `app.R:2013-2034`) und "nie angefasst" dasselbe `NULL` zurück. `resolve_vocabulary_slot_overrides()` (856-866) überspringt `NULL`; die Spalte behält `values_from:` und wird zum vollen Vokabular. Zusätzlich behandelt `vocabulary_values()` (480-484) mit dem `%||%` der App (zero-length = fehlend) eine leere Termliste als "alle Terme".

**Änderung (nach D1 (a)).**
1. `vocabulary_slot_values()` liefert für eine bewusst leere Auswahl einen unterscheidbaren Wert (z. B. `list(clear = TRUE)`), nicht `NULL`.
2. `resolve_vocabulary_slot_overrides()` setzt dafür `values` und `values_from` der Zielspalte auf `NULL` (kein `character(0)`: ein leeres Atomic-Vektor-`values` bricht `as_json_schema()`, `V3` hat das nachvollzogen).
3. `vocabulary_values()`: `if (is.null(terms)) terms <- vocab$terms`, damit eine leere Liste nicht mehr zur Gesamtmenge wird. In `mode: open` mit nur eigenen Begriffen liefert die Funktion genau diese.
4. Ergänzung (b) von D1: **zuerst prüfen**, ob der Dialog `min` heute schon durchsetzt (in `app.R` und `vocabulary.R` nach `min` suchen). Nur falls nicht, eine Prüfung ergänzen, und zwar als reine Funktion (`vocabulary_slot_selection_problem(slot, selection)`), die die Auswahl gegen `min`/`max` prüft. So lässt sie sich direkt testen, weil Dialog-Verhalten in `testServer` nicht sichtbar ist (`showModal` und `update*Input` sind dort Attrappen).
5. Den falschen Kommentar in `app.R:2013-2019` und im `vocabulary.R`-Kopf ersetzen.

**Tests (zuerst rot).**
- Ende-zu-Ende: `create_dta_from_template()` mit `vocab_selections = list(visit_choice = character(0))` ⇒ die gebaute Spalte hat **kein** `values` und keinen `values_from`-Verweis. Erwartung von Hand geschrieben (`c("SCR","C1D1","EOT")` ist heute die falsche Antwort, steht in `test-shinyapp-vocabulary.R:1232-1250`).
- `mode: open`, Auswahl `"UNSCHED"` ⇒ `values == "UNSCHED"`, kein Rest des Vokabulars.
- Die beiden benannten Tests (`test-shinyapp-vocabulary.R:1345-1364`, `test-shinyapp-template-ui.R:1089-1125`) so erweitern, dass sie das gebaute Dokument prüfen, nicht nur das Zwischen-`NULL`.
- `min`-Slot leer ⇒ Dialog meldet einen Fehler.

**Qualifikation.** Betrifft `REQ-APP-017` (laut `q2-qual-tests-a-13` ist sie ohnehin ohne wirksame Verifikation): neuer OQ-Fall mit echtem Eingabefeld `col_vocab_ref`; diesen Fall in den Plan der major-Befunde, nicht in diesen PR.

## 6. PR D: `pattern` und leerer String (`q3-qual-tests-b-27`)

**Ursache.** `dta_condition_mask()` (`R/evaluateRules.R:851`): `grepl(pattern, as.character(x), perl = TRUE)`. Für `""` ist `grepl("^$", "")` wahr. OQ-RULE-011 prüft die Behauptung mit `^$` auf `c("x", NA)`, also einer Spalte ohne leeren String, und kann nicht scheitern.

**Änderung (nach D2 (a)).**
1. Pattern-Zweig: `res <- grepl(...); res[is.na(x) | (is.character(x) | is.factor(x)) & !nzchar(as.character(x))] <- FALSE`. Das Verhalten gilt für IF- und THEN-Klauseln gleichermaßen. **Als Prüfschritt, nicht als Annahme:** dass eine THEN-`pattern`-Prüfung bei `NA` heute schon als Verstoß zählt, im Code nachlesen (`rule_check_col_condition()` und `dta_condition_violated()`, `evaluateRules.R:1251-1287`), bevor der Fix behauptet, `""` werde damit wie `NA` behandelt.
   - **Dieselbe Zeile 851 trägt einen weiteren bestätigten Befund** (`c2-data-25`): `pattern` auf numerischen Spalten rendert über `as.character()`, `100000` wird `"1e+05"`. Empfehlung: **im selben PR beheben** (die Zeile wird ohnehin angefasst; numerische Spalten über `dta_numeric_text()` rendern, wie es die Spaltenprüfung `pattern` schon tut) und mit einem eigenen, von Hand abgeleiteten Test pinnen (`100000` mit Muster `^100000$` ⇒ trifft, `0.00001` mit `^0\\.00001$` ⇒ trifft). Wenn du das trennen willst, dann bewusst und mit Verweis, damit der zweite Fix nicht an der Zeile vorbeigeht.
   - Zusätzlich ein Check, dass die **Spaltenspezifikations-`pattern`-Engine** (`columnSpecChecks.R`) einen leeren String nach der Änderung gleich behandelt wie der Regeloperator; sonst haben beide Engines wieder zwei Verdikte für denselben Wert.
2. Das gilt auch für alle Aufrufer von `dta_condition_mask()`; per Grep auf die Aufrufstellen prüfen (Regelfunktionen, Gruppenregeln, Streaming-Pfad), damit der Streaming-Pfad dieselbe Maske nutzt. **Parität prüfen**, weil das Projekt den eifrigen und den Streaming-Pfad gegeneinander testet (`t-core-1`/`c3-stream`).
3. Roxygen des Operators und `DTA_YAML_GUIDE.md` an der Stelle ergänzen: leere Strings und fehlende Werte matchen nie, `empty` ist der Operator dafür.

**Tests (zuerst rot).**
- `tests/testthat/test-evaluateRules.R`: Spalte `c("", NA, "x", "AB1")`, Muster `^$`, `.*`, `^[A-Z]*$`, `^[A-Z]+[0-9]$`; erwartete Auswahl **von Hand** aus der Anforderung: `""` und `NA` nie, `x` nur bei `.*` und `^[A-Z]*$`.
- Streaming-Parität: dieselbe Tabelle durch den eifrigen und den Streaming-Pfad, gleiches Ergebnis.
- OQ-RULE-011 neu: das Fixture enthält `""`, das Muster ist `.*` und `^$`, die Erwartung steht in der Anforderung ("matcht nicht"), kein beobachteter Wert. Das ist die Kernregel der Suite: *ein Erwartungswert stammt nie aus der Software.*
- Mutationsprobe: die neue Zeile entfernen ⇒ beide Tests werden rot.

**Weitere Dokumente.** `deviations.yaml` unverändert (der Fix schließt die Lücke, es gibt keine Abweichung). Falls D2 (b) gewählt wird, stattdessen REQ-RULE-007 umformulieren, einen Fall für das tatsächliche Verhalten schreiben und kein Produkt ändern.

## 7. PR E: OQ-ROBUST-012 (`q3-qual-tests-b-17`)

**Ursache.** `test-OQ-ROBUST-inputs.R:194-208`: `outcome` kann nur `"condition"`, `"no table"` oder `"validated"` sein, die Prüfung akzeptiert alle drei und `nzchar(outcome)` ist immer wahr. REQ-ROBUST-012 ("gemeldet statt still umgedeutet") ist damit "verified", egal was `load_file()`/`check()` mit ungültigem UTF-8 tun. Heute lehnt die Software die Bytes korrekt ab (der Lauf hat `"condition"` aufgezeichnet), der Test beweist es nicht.

**Änderung (nur Test).**
1. Vertrag aus dem Anforderungstext ableiten (nicht aus dem Lauf): *entweder* eine Bedingung wird geworfen, *oder* der Befund erscheint (`ok == FALSE` und mindestens eine Meldung zur betroffenen Zeile/Spalte), *und* der gelesene Wert ist nicht der ersetzte Text (kein U+FFFD, kein gekürzter Wert).
2. Die Klasse der Bedingung prüfen (Konvention des Projekts: keine Prüfung auf übersetzten Text). Vor dem Schreiben im Code nachsehen, ob die Ablehnung eine Klasse hat (Notiz in `t-core-2`: die "classless refusal" gilt für breite Kodierungen; für ungültige Bytes lesen).
3. Mutationsprobe als einmaliger Begleitbeweis, **nicht** als Teil irgendeiner Suite: lokal in einem Wegwerf-Worktree den Reader so ändern, dass er `U+FFFD` einsetzt; der neue Fall muss rot werden, der alte blieb grün. Das Ergebnis kommt in die PR-Beschreibung. Ein dauerhafter Negativkontrolltest gegen einen absichtlich ersetzenden Reader wäre eine Attrappe der getesteten Funktion, also genau das, was das Review als Cheating gewertet hat; deshalb entfällt er.

## 8. Gemeinsame Regeln für alle PRs

- **Reihenfolge je PR:** (1) Test schreiben, der auf dem unveränderten Code fehlschlägt, Fehlschlag protokollieren; (2) Fix; (3) Test grün; (4) Mutationsprobe; (5) `Rscript .github/scripts/style.R`; (6) `roxygen2::roxygenise()` ohne Diff (nur bei PR D relevant); (7) `inst/` geändert ⇒ `Rscript .github/scripts/sync_qualification_manifest.R`; (8) die betroffenen testthat-Dateien und die betroffenen OQ-Fälle laufen lassen; (9) `CHANGELOG.md` unter `## [Unreleased]` (Fixed); (10) Qualifikations-Dokumentation lesen: `inst/qualification/docs/maintaining.md` zuerst, weil neue Anforderungen mit Tests, Meta-Checks und Manifest zusammenhängen.
- **Worktree-Besonderheit:** pro Worktree eine untracked `.Renviron` mit `RENV_CONFIG_AUTOLOADER_ENABLED=FALSE`, R 4.5.1 mit absolutem Pfad.
- **Arbeitsteilung:** Planung auf dem Haupt-Thread; Testläufe nie im selben Prozess wie die Planung; Diffs werden vor dem Commit gegengelesen.
- **Limit-Vorsicht:** höchstens zwei PRs gleichzeitig in Arbeit (siehe D4); das 5-Stunden-Limit hat den Review-Lauf fünfmal abgebrochen.
- **PR-Ziel:** immer `dev`, nie ein Feature-Branch (der File-drop-Fix ging so verloren). `gh pr checks` meldet auf grünen PRs "no checks reported" (Manifest-Sync); mit `gh run list --branch` warten, bis **alle** Workflows (`R-CMD-check` auf allen Legs, `r-style`, `pre-commit`, bei `inst/` auch `qualification`) fertig und grün sind, **bevor** ein Merge vorgeschlagen wird. Gemergt wird nur auf deine Anweisung.
- **Merge-Reihenfolge:** A → C → D → E → B. Nach jedem Merge die offenen PRs auf `dev` rebasen, das Qualifikationsmanifest neu erzeugen (zweite `inst-files.sha256` bricht sonst `r-style`).

## 9. Risiken und offene Punkte

- **PR B ist der größte Eingriff** (neue Datei, Umbau der Formularzeilen, neuer Zustand je Zeile). Er verändert die Oberfläche für Autoren, die mehrere Operatoren auf einer Spalte haben: das Formular zeigt mehr Zeilen als zuvor. Risiko für bestehende `testServer`-Tests, die die Zeilenzahl pinnen; die Tests zuerst lesen.
- **PR A** ändert, wann "grün" erscheint. Es gibt Datasets mit zugehörigen `skipped`-Tabellen; Semantik vor dem Codieren klären.
- **PR D** ändert eine Regelsemantik. Bestehende Spezifikationen, deren THEN-`pattern` leere Strings stillschweigend akzeptierten, schlagen künftig an (vor 1.0.0 vertretbar). Im CHANGELOG deutlich nennen.
- **Browser-Verhalten** von `<select>` und Dialogen ist nur im echten Browser zu belegen (PR B, PR C).
- Die Gegenprüfung hat die Befunde durch Lesen bestätigt; ausgeführt wurde außer den vorhandenen Suiten nichts. Schritt (1) jedes PR (der Test, der **vor** dem Fix scheitert) ist deshalb zugleich die Laufzeitbestätigung des Befunds. Scheitert er nicht, wird der Befund neu bewertet, bevor repariert wird.
