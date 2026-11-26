# DTAtools — Gesamtbericht zum Code-Review

Stand: 21.–23.11.2026. Das Review liefert Befunde, keine Fixes; im Repository wurde nichts verändert.

## 1. Umfang, Methode, Grenzen

- **Geprüfter Stand:** Git-Tree `68408c44` = `origin/dev` @ `ec76a8f5` + File-drop-Fix `9f6c0d3d`. Ein Cherry-Pick desselben Fixes ist inzwischen als PR #226 in `dev` gemergt (`3baa60e4`). Der Fix aus PR #225 war zuvor in einen bereits gemergten Feature-Branch gelaufen und hatte `dev` nie erreicht.
- **Reviewer:** 16 Lese-Reviewer (je Bereich ein Leser), jeweils mit festem Brief (was als Cheating zählt, Befundformat, nur lesen, nichts ausführen). Dazu 1 Runner, der als Einziger R ausgeführt hat, und 6 unabhängige Verifizierer. Das 5-Stunden-Limit hat die Reviewer mehrfach abgebrochen; sie wurden jeweils fortgesetzt, die Befunde standen schon in Dateien.
- **Rohbefunde:** 735 (11 blocker, 99 major, 440 minor, 185 nit). Aufschlüsselung nach Bereich in Abschnitt 7, vollständige Liste in `REVIEW-APPENDIX-all-findings.md`.
- **Gegenprüfung:** Alle 110 Befunde mit Schwere blocker/major wurden von einem zweiten Leser mit dem Auftrag geprüft, sie zu widerlegen: **70 bestätigt, 36 bestätigt mit Korrektur (meist Schwere nach unten, teils Beschreibung), 4 per Lesen nicht entscheidbar, 0 widerlegt.** Die rund 625 minor/nit-Befunde sind **nicht** gegengelesen.
- **Grenzen:**
  - Die Reviewer haben nichts ausgeführt (kein R, kein Browser). Aussagen über Laufzeitverhalten sind im jeweiligen Befund als `inferred` markiert.
  - Dass kein einziger geprüfter Befund widerlegt wurde, spricht für die Qualität der Erstbefunde, kann aber auch heißen, dass die Verifizierer den Befunden zu nahe folgten. Die 36 Korrekturen zeigen, dass sie nicht blind bestätigt haben.
  - Nicht oder nur oberflächlich gelesen: rund 200 kleine YAML-Fixtures unter `inst/qualification/fx/` und etwa 50 Template-Fixture-Ordner unter `fx/t`; Binär-Fixtures nur über Generatoren und Hashes; Teile von `private-templates.Rmd` (Zeilen 1131–1511) und rund 900 Zeilen des Skeleton-READMEs nur mechanisch; Referenzdateien der Agent-Skills nur mechanisch; CHANGELOG ab Version 0.27.0 abwärts nur strukturell; etwa 470 `man/*.Rd`-Seiten nicht gegen roxygen verglichen (das übernimmt der Runner, siehe unten).
  - Die Tests der Code-Slices haben die Code-Reviewer nicht auf Cheating geprüft; das haben die Test-Reviewer getan.

## 2. Was die objektiven Läufe zeigen (Runner, R 4.5.1, S7 0.2.2)

| Prüfung | Ergebnis |
| --- | --- |
| roxygen2 8.1.0 neu erzeugen, Diff gegen den Stand | 0 abweichende Dateien (`man/`, `NAMESPACE`, `Collate:`) |
| Style (`style.R --check`, leerer Cache) | 338 Dateien sauber; die weiteren `r-style`-CI-Gates ebenfalls |
| testthat, komplett | 163 Dateien, 35.914 Erwartungen, **0 Fehlschläge, 0 Fehler**, 4 Warnungen, 8 Skips, 76,6 Min |
| Qualifikation, Stufe `quick` | `PASS WITH NOT-EXECUTED TESTS`: 812 Fälle, 808 bestanden, 4 nicht ausgeführt, 14/14 Meta-Checks; 581 Anforderungen, 575 verified, 2 verified with deviation, 4 nicht verifiziert |

**Bedeutung:** Alle Läufe sind grün, obwohl die Reviews unten reale Fehler und viele Tests ohne Aussagekraft zeigen. Das ist selbst ein Befund: Die heutigen Suiten fangen diese Fehler nicht.

**Eigener Befund des Runners (objektiv, nicht gelesen sondern beobachtet):** In `tests/testthat/test-DTAFile.R` ("a pattern description survives the factory and a YAML round trip", Zeilen 1222–1342) stecken 12 `test_that`-Blöcke ineinander. Ein fehlschlagender `expect_*` vor einem verschachtelten Block geht im Ergebnisobjekt verloren (die Ausgabe zeigt `[ FAIL 1 ]`, `stop_on_failure = TRUE` wirft aber nicht). Ein Fehlschlag in Zeile 1228 würde die Suite also nicht rot machen.

**Skips (8):** 2× `DTATOOLS_LOCAL_LIBRARY` nicht gesetzt (nichts in `.github/` setzt sie, die CI überspringt also ebenfalls), 3× Symlinks nicht verfügbar, 1× Windows-Plattform, 1× chromote-Start (beide Browser-Tests liefen bei einem Zusatzversuch durch), 1× `git show f6189bf:...` in `test-DTADataSetMatrix.R:936` (die CI-Checkouts haben keine `fetch-depth`; Überspringen dort nur vermutet). **Qualifikation:** nicht ausgeführt wurden IQ-ENV-019, OQ-TMPL-097, PQ-PERF-024/-025; in der Stufe `quick` sind REQ-PERF-009/-010 (Hochrisiko) nicht verifiziert; OQ-EXPORT-010 meldet eine `cbind`-Recycling-Warnung unbekannten Ursprungs.

## 3. Antworten auf deine fünf Hauptpunkte

1. **Tests, die cheaten oder hartcodiert ausweichen:** Ja, in beiden Suiten. 32 Befunde der Kategorie `test-cheat` und 224 der Kategorie `test-weak`. Muster: Fälle, die nicht scheitern können (akzeptierte Menge = Wertebereich des Ausdrucks), Vergleiche eines Laufs mit sich selbst, Skip als einziger Fehlerpfad, Tests von Harness oder Fixture statt des Produkts, Anforderungen aus beobachtetem Verhalten abgeschrieben (Verstoß gegen die Regel "ein Erwartungswert stammt nie aus der Software"). Details in Abschnitt 5.
2. **Code, der Fehler umgeht:** 53 Befunde `code-cheat`. Gemeinsames Muster: "nicht geprüft" oder "Fehler aufgetreten" wird still zu "ok" bzw. "keine Befunde" (Abschnitt 4 und 6).
3. **Dokumentation:** 123 Befunde `doc-wrong`, 16 `doc-missing`. Sie ist nicht perfekt; bestätigt sind u. a. falsche generierte Hilfe zu `check()`, ein dokumentierter `ok`-Eintrag in `results()`, den es nicht gibt, falsche Aussagen zu `max_errors`, veraltete JSON-Schema-Aussagen und eine doppelt kopierte CHANGELOG-Passage (Abschnitt 4.6).
4. **Abwärtskompatibilität:** Nicht als Mangel gewertet. 63 Befunde `dead-code`, darunter ein ganzes Kompatibilitätssubsystem (`upgrade_dta()`, `dta_is_stale()`, tolerante Accessoren, Artefakt-Migration; `t-core-3-17`, `c2-06`, `c2-37`; es läuft an jedem Einstiegspunkt und ist qualifiziert (REQ-CONT-029, REQ-MATRIX-059), ist nach deiner Regel aber überflüssig) sowie `dataset_only`, den `prov$selections`-Replay-Zweig und alte Template-Listing-Helfer in der Shiny-App. Vor 1.0.0 sind das Löschkandidaten.
5. **Alle Fehler und Denkfehler:** Geprüfte Blocker und Majors stehen in Abschnitt 4; alles Übrige in den Findings-Dateien und im Anhang.

## 4. Geprüfte Befunde mit Schwere blocker oder major

Schwere in Klammern ist die nach der Gegenprüfung.

### 4.1 Blocker (8 bestätigte Befunde in 7 Themen, 1 offen)

1. **Regel-Editor schreibt Operatoren um** (`s1-app-01`, `app.R:6399-6427`): Öffnen und Speichern einer Regel mit `range`, `not_equal` oder `equal` macht daraus `equals`. `not_equal` wird damit ins Gegenteil verkehrt; `range: [0.5, 40]` aus dem eigenen Guide wird zu `equals: "0.5, 40"`.
2. **Mehrere Operatoren je Spalte gehen verloren** (`s1-app-02`, `app.R:6405-6420`): Eine Klausel mit mehreren Operatoren auf einer Spalte, die der Guide vorschreibt, überlebt den Editor nicht; beim Speichern überschreiben sich Zeilen.
3. **Regelwerte werden beim Speichern verfälscht** (`s1-app-03`, von V5 auf blocker angehoben, `app.R:6428-6457`): Werte laufen durch `as.numeric()` und Komma-Join/-Split. `"001"` wird `1`, `"Smith, John"` wird zu zwei Werten, auch ohne Bearbeitung.
4. **Status-Map meldet "Passed" über ungeprüften Tabellen** (`s1-app-28`, `s2-shiny-r-03`, `utils_dta.R:1268-1277`, `app.R:4697-4705`): Ein Dataset gilt als bestanden, wenn eine Tabelle bestanden hat, auch wenn eine zweite gebundene Tabelle nie validiert wurde. Die zertifizierende Zusammenfassung wird dann angeboten ("VALIDATION PASSED"). Die Regel des Pakets selbst wertet das als unvollständig; die App-Kopie ist abgedriftet.
5. **Leerer Vokabular-Slot ("None") ergibt das ganze Vokabular** (`t-shiny-1-14`, `s2-shiny-r-20`/`-21`, `vocabulary.R:776-812, 856-866`): `vocabulary_slot_values()` liefert für "geleert" und "unberührt" dasselbe `NULL`; das `%||%` der App behandelt zero-length als fehlend. Der Kommentar in `app.R:2013-2019` ist falsch, zwei Tests prüfen nur das Zwischen-`NULL`. Was "None" erzeugen soll, ist im Code selbst nicht definiert.
6. **OQ-ROBUST-012 kann nicht fehlschlagen** (`q3-qual-tests-b-17`): Die akzeptierte Ergebnismenge ist exakt der Wertebereich des Ausdrucks, der `outcome` erzeugt. Das Produkt lehnt die schlechten Bytes heute korrekt ab; der Test beweist es nicht.
7. **OQ-RULE-011 prüft das Falsche, und das Produkt verletzt REQ-RULE-007** (`q3-qual-tests-b-27`): "Ein leerer String passt nicht auf ein Muster" wird mit `^$` auf `c("x", NA)` geprüft. Die Software matcht den leeren String gegen `^$`, der Code hat keine Leerstring-Regel; keine Abweichung ist registriert, die Evidenz sagt "verified". Das ist der einzige Blocker, der einen echten Produktfehler und einen irreführenden Test zugleich zeigt.
8. **Offen, nicht entscheidbar** (`t-shiny-2-10`): Der einzige Test des `suspendWhenHidden = FALSE`-Vertrags besteht nach Aussage des Reviewers immer, weil `outputOptions()` unter `testServer()` in shiny 1.14.0 ein No-op sei. Aus dem Snapshot lässt sich das nicht lesen, und die erwartete Meldung "outputOptions is not fully implemented by MockShinySession" taucht im Lauf-Log nicht auf (vereinbar mit stillem No-op, beweist aber nichts). Entscheidend ist ein Minimaltest: `outputOptions()` innerhalb eines `testServer` aufrufen. Bei Bestätigung wäre es major. Die Blindstelle `isTRUE(NULL)` im Test selbst ist real.

Von den ursprünglichen Blockern wurden drei auf **major** zurückgestuft: `c4-tmpl-export-01` (Traceability, siehe 4.4), `q2-qual-tests-a-17` (Produkt ist korrekt, der Test kann den Import-only-Verstoß aber nicht isolieren) und `t-core-1-26` (die OQ-Paritätsfälle vergleichen Zeilen und Spalten im Kleinen bereits).

### 4.2 Majors: Engine, Spezifikationsmodell, Datenschicht

- **Bares Backend (`type: SAS`) baut und lintet sauber**; beim Check werden alle Wertprüfungen der Spalte hinter einer unbehandelten Warnung übersprungen, die Tabelle gilt als gültig (`c1-spec-16`). Dasselbe Muster bei jedem Schema-Kompilierfehler: nur eine Warnung, der Test pinnt die Warnung, nie das Verdikt (`t-core-2-36`).
- **`type: file`-Dataset verwirft still `columns:` und `rules:`**; die spiegelbildlichen Fehlplatzierungen bei Matrix-Datasets sind harte Fehler (`c1-spec-17`).
- **Schlüssel von Gruppenbedingungen werden nie geprüft**: Ein falsch geschriebenes `*_scope` wird still zu `"any"`, `cst$left` trifft per Partial-Matching `left_scope` (`c1-spec-49`).
- **Spec-Schreiber runden Zahlen** (`write_columns_to_json()`/`_yaml()`, `as_json_schema()` ohne Präzision: jsonlite 4 Nachkommastellen, yaml 7 signifikante Stellen); der Rest ist über die Bibliotheksdoku belegt, nicht ausgeführt (`c1-spec-36`).
- **Regeloperator `pattern` auf numerischen Spalten** nutzt `as.character()`, also `100000` → `"1e+05"`; die `pattern`-Prüfung der Spaltenspezifikation vermeidet das (`dta_numeric_text()`); in IF-Teilen fallen Zeilen still aus dem Geltungsbereich (`c2-data-25`).
- **Tabelle nur mit Kopfzeile** bekommt `ok = TRUE` und "passed validation", obwohl alle Wertprüfungen `not_checked` sind. Bewusste, kommentierte Entscheidung, aber in sich widersprüchlich (`c2-data-28`, Designfrage).
- **Wörtlicher Dateieintrag mit mehreren Namen** gilt im Datei-Dataset als erfüllt, wenn ein Name geliefert wurde; die Tests dazu laufen alle mit `pattern = TRUE`. Die Semantik ist im Code selbst widersprüchlich (`c2-data-44`).
- **`t-core-3-37`:** Spaltenmappings aus YAML gehen ohne Unbekannt-Schlüssel-Prüfung an `do.call(DTAColumnSpec, raw)`.
- **Streaming/Reporting:** `summarised_error` entsteht aus dem gekappten Kopf und zählt über `max_errors` hinaus zu wenig (`c3-stream-01`); `inspect()`/`explain_messages()` berechnen die Zeilen eines gestreamten Datasets aus ungetyptem Rohtext neu und widersprechen dem Verdikt (`c3-stream-02`); `inspect()` bricht bei einer nicht auswertbaren IF/THEN-Regel ab (`c3-stream-12`); `inspect()` scannt je Meldung neu (`c3-stream-13`, Performance, ungemessen); die "Non-numeric values"-Hinweise in `rulePhrasing` beschreiben das Verhalten auf ungetyptem Text und sagen für deklariert numerische Spalten das Gegenteil (`c3-stream-25`).
- **S7-Pin** (`c1-spec-31`, `c2-data-50`, `d1-userdocs-35`, `d2-meta-ci-01`, ein Befund in vier Fassungen): Zehn Konstruktoren rufen `new_object(.parent = ...)` mit Namen auf, die CI pinnt in sechs Schritten exakt `cran::S7@0.2.2`, `DESCRIPTION` erlaubt jedes spätere S7, die Installationsanleitungen nennen den Pin nicht. **Verifiziert sind alle Aufrufstellen und Pins; ob ein aktuelles S7 den benannten Aufruf wirklich bricht, ist aus dem Snapshot nicht entscheidbar** (Test: aktuelles CRAN-S7 in sauberer Bibliothek installieren, `DTARuleColRange(id = "r", columns = "A", range = c(1, 2))` aufrufen, `args(S7::new_object)` ansehen). Der "ohne Begründung"-Teil von `d2-meta-ci-01` ist zu stark: `CHANGELOG.md:5224` nennt einen allgemeinen Grund.

### 4.3 Majors: Templates, Export, Shiny

- **Template/Export:** PDF-Export fällt still von Word/LibreOffice auf TinyTeX/pandoc zurück (verliert Kopf-/Fußzeile) (`c4-16`); offene Dateianzahl wird als "2 to 0" exportiert (`c4-22`); nicht gerenderte Block-Platzhalter und kaputte `{#DATASETS}`-Marker werden nur bei `quiet = FALSE` gemeldet, `write_dta(template=)` und die App übergeben immer `quiet = TRUE` (`c4-29`); das Guide-Beispiel baut für `required: true`-Party-Slots kein Datasets-Kapitel, `validate_template()` lässt sie durch (`c4-45`, laut Verifizierer laut, nicht still).
- **Template-Engine der App:** Ein Kind-Template kann die Werte einer versiegelten Spalte über einen Vokabular-Slot überschreiben (`s2-shiny-r-19`, REQ-TMPL-021 prüft nur Optionen und Party-Slots); Rebase schreibt positionelle Metadaten-Schlüssel nach Namen (`s2-shiny-r-25`); Templates mit `required: true`-Party-Slot lassen sich nie rebasen oder in der Historie berechnen (`s2-shiny-r-38`).
- **App-Logik:** Ungültige Regelgrenzen werden still verworfen statt gemeldet (`s1-app-04`); Umbenennen einer benannten Bedingung hebt sie in allen Constraints auf (`s1-app-05`); das Lande-Ziel hat keine Größen-/Typprüfung bis zum 1-GB-Limit (`s1-app-06`); `dv_findings()` macht jeden Fehler von `container_findings()` zu "keine Befunde", gelesen als "Packaging conforms" (`s1-app-07`, auslösende Eingabe nicht ermittelt); fehlgeschlagene Autosaves werden verschluckt (`s1-app-13`).
- **Nicht entscheidbar (Sicherheit):** Vokabular-Termlabels und Option-IDs landen unmaskiert im inline-JSON-`<script>`-Block von `selectizeInput`; ein `</script>` im Bibliotheksinhalt wäre vermutlich ein XSS-Weg. Wie shiny 1.14.0 das serialisiert, ist aus dem Snapshot nicht lesbar (`s2-shiny-r-45`).
- **Zur File-drop-Änderung:** Die neue Flag-Logik stimmt bei Hinzufügen, Entfernen, Umbenennen, Standard-Container, Reset und Restore (`s1-app`). Ein Mangel: In einem Dokument ohne Container ist der Delivery-Block im Ansichtsmodus ausgeblendet, selbst wenn eine per Slot abgelegte Archivdatei eine "Last delivery"-Zeile hinterlassen hat (`s1-app-09`, nicht Teil der Gegenprüfung). Tests: `test-shinyapp-containers.R` pinnt die Regel in beide Richtungen; `test-shinyapp-delivery.R` testet sie nie; die `suspendWhenHidden`-Prüfung in `test-shinyapp-dataset-add-remove.R` kann nicht fehlschlagen (s. 4.1 Nr. 8).

### 4.4 Majors: Qualifikationssuite (Infrastruktur und Verdikt)

- **Traceability** (`c4-tmpl-export-01`, `q1-qual-infra-01`, auf major zurückgestuft): `qual_traceability()` markiert eine Anforderung als `verified`, sobald **ein** zugehöriger Fall bestanden hat, auch wenn die anderen übersprungen wurden. README, `maintaining.md` und Validierungsplan versprechen das Gegenteil; der Plan (`PLAN-qualification-suite.md:200`) hatte es absichtlich als "≥ 1 bestanden" definiert, es ist also Drift zwischen Code und veröffentlichten Dokumenten. Das Gesamtverdikt (`PASS WITH NOT-EXECUTED TESTS`) und Abschnitt 9 zeigen die Skips weiterhin. Beispiel: REQ-EXPORT-040 steht auf "verified", wenn OQ-EXPORT-060/-063/-064 ohne pandoc überspringen. Kein Meta-Check erfasst es (M10 zählt Skips als ausgeführt), und kein Test baut eine Anforderung aus bestandenem plus übersprungenem Fall.
- `q1-qual-infra-19`: Das `affects:`-Feld einer Einschränkung erreicht die Matrix nie; REQ-VAL-026 und REQ-PERF-011 sind `verified`, obwohl ihre Fälle das Gegenteil festschreiben.
- `q1-qual-infra-16`: 13 von 14 Meta-Checks haben in keiner Suite einen Test, der sie fehlschlagen lässt.
- `q1-qual-infra-03` / `q2-qual-tests-a-01`: IQ-ENV-016 (Manifest-Integrität) und IQ-ENV-019 (Vignetten) überspringen genau dann, wenn ihre Evidenzbasis fehlt; IQ-ENV-019 hat zusätzlich eine tautologische Restprüfung. Ob die CI-Installation Vignetten listet, braucht einen Lauf (`R CMD INSTALL --build . && Rscript -e 'nrow(vignette(package="DTAtools")$results)'`).
- `q1-qual-infra-08`: Der Skalen-Generator schreibt bei Zeile 10.000.000 `S10000000` und bricht damit seine eigene Längenvorgabe; in der Stufe `full` (Standard von `run_qualification()`) liegt das Orakel um zwei Fehler daneben, die CI läuft diese Stufe nie. Dass es wirklich fehlschlägt, ist nicht ausgeführt.
- `q1-qual-infra-29`: Anforderungen wurden aus beobachtetem Verhalten abgeschrieben, also stammen Erwartungswerte aus der Software, entgegen der Leitregel.
- `q2-qual-tests-a-08`: Das Verdikt wird nur mit Status `fail` geprüft, nie mit `error` oder `file_error`.
- Weiterhin bestätigt: weitere, deutlich schmalere Befunde (`q1-02/-26/-27/-28`, `c4-02/-03`) wurden auf minor zurückgestuft: Der Bericht druckt `sha256sum -c ` ohne Dateinamen (Zeile `qualification.R:1991` vor 1993; `SUMMARY.txt` und `SHA256SUMS` stimmen); ein Lauf ohne Stufen endet als `PASS (PARTIAL)`, ist aber ein dokumentierter Modus.
- **Zusätzlich (`d2-meta-ci-07`, Schwere minor, nicht gegengelesen):** Das Qualifikations-CI-Gate schlägt nur bei `^FAIL` an; `PASS WITH NOT-EXECUTED TESTS` und `PASS (PARTIAL)` gehen grün durch, und der Workflow installiert kein TinyTeX.

### 4.5 Majors: Tests, die nichts oder zu wenig prüfen

**Qualifikationssuite, Fälle, die nicht scheitern können oder nur einen Teil der Anforderung abdecken (alle bestätigt):**

- *Kann nicht fehlschlagen:* `q2-13` (setzt ein Eingabefeld, das die App nicht hat, wählt keinen Term, prüft nur "nichts abgebrochen"; REQ-APP-017 unverifiziert), `q2-14` (die "nennt jedes fehlende Paket"-Prüfung wird vom Produktnamen "DTAtools" erfüllt), `q2-15` (OQ-APP-002 ruft `run_dta_app()` nie auf), `q3-20` (läuft mit `persist = FALSE`, prüft dann ein frisches Verzeichnis), `q3-28`/`-29` ("C-Collation-Reihenfolge" und "nennt seine Gruppe" sind `grepl` auf zusammengeklebten Text, den schon das `FALSE` von `rows_truncated` erfüllt), `q3-36` (akzeptiert beide möglichen Ausgänge), `q3-37` (Kommentar nennt das Hazard "007", das Fixture schreibt die Werte gequotet), `q3-42` (kann "Arrow stimmt mit R überein" nicht von "Arrow lief nie" unterscheiden), `q3-56` und `q3-59` (vergleichen einen Lauf mit sich selbst: `use_threads` wird bei In-Memory-Tabellen ignoriert, die Arrow-Option wird auf dem Streaming-Pfad nie gelesen), `q3-62` und `q3-60` (einziger Fehlerpfad ist ein Skip; eine App, die nicht startet, ergibt `PASS WITH NOT-EXECUTED TESTS`; `q3-60` auf minor), `q3-66` (kann nicht scheitern, halbe Anforderung nicht gemessen).
- *Deckt nur einen Teil ab:* `q2-16`, `q2-18` (REQ-APP-050 Pfadform), `q2-34` (REQ-EXPORT-032: 1 von etwa 7 Funktions-/Formatkombinationen), `q2-35` (positive Hälfte von "overwrite = TRUE ersetzt" nirgends geprüft; für `write_dta`/`write_dataset_metadata` fehlt sie schon in den Anforderungen), `q3-01`, `q3-02`, `q3-07`, `q3-13`, `q3-18`, `q3-21`, `q3-30`, `q3-38`, `q3-47`, `q3-48`, `q3-57`, `q3-63` (Korrektheit im Großen nur über Zähler, Injektionsplan verworfen; falsche Zeilennummern und der Verlust der letzten ~11 % einer Lieferung bestehen), `q3-64` (Speichergrenze sieht Arrow-seitiges Wachstum nicht und endet bei 1e6 Zeilen), `q3-65` (von sieben Messgrößen wird eine per Wert geprüft), `q3-68`, `q3-69`.
- *Kleinere Korrekturen:* `q3-19/-22/-34/-35` auf minor (OQ-EXPORT-034 und -032 prüfen mehr als behauptet; was offen bleibt, sind Mutationen von `labels()`/`get_table()`).

**testthat:**

- `t-shiny-1-01`: Der Test "HTML validation report download handler produces parseable HTML" ruft den Download-Handler nie auf; er ruft `write_validation_report()` selbst auf und durchsucht `app.R` als Text.
- `t-shiny-2-14`: Mindestens 16 der 70 Outputs der App rendert kein Test (u. a. `dl_yaml`, `dl_validation_summary`, das Add-Dataset-Panel); ein Absturz dort bliebe grün.
- Weitere bestätigte Test-Schwächen bei Blocker/Major-Schwere: `t-core-2-36` (siehe 4.2), `t-core-3-17` (Kompatibilitätssubsystem für alte `saveRDS()`-Objekte: kein toter Code, sondern aktiv und mit Anforderungen und Tests belegt; vor 1.0.0 löschen, dann entfallen auch diese Anforderungen und Tests, deren Harness in drei Testdateien ein altes Objekt nachbaut). Dazu der objektive Runner-Befund aus Abschnitt 2 (verschachtelte `test_that`).

### 4.6 Majors: Dokumentation, Build, Benchmarks

- **`?check` und andere generische Hilfeseiten** behalten nur einen `\item{...}` der Methoden (`man/check.Rd`, `load_file.Rd`, `read_file_execution.Rd`): `?check` nennt `max_errors`/`fail_fast` "Accepted and ignored" und lässt die Argumente des tabellarischen Falls weg (`c2-data-01`). roxygen-Drift gegen den Stand gibt es keinen, die Rd-Seiten sind also genau so erzeugt.
- **Vignette** sagt, `max_errors` werde für Tabellen im Speicher ignoriert; tatsächlich kappt es die Details bei 10.000 (`validationFunctions.R:352-353`), `messages()` und `inspect()` sehen nur die gekürzten Frames (`d1-userdocs-20`).
- **`docs/index.html`** dokumentiert eine Spalte `ok` in `results()` und empfiehlt `if (results(dta)$ok)`; die Methoden liefern 16 Spalten ohne `ok`, der Code bricht ab (`d1-userdocs-26`).
- **Benchmark `bench_streaming.R`:** `tbl` und `typed` des Eager-Laufs bleiben während der Streaming-Messung am Leben, `streaming.csv` kann beschränkten Speicher nicht belegen (`d2-meta-ci-24`, bestätigt: `stream_peak_mb` wächst um +98 und +184 MB wie `eager_hold_mb` mit +85 und +178 MB). Angrenzend: "~19 MB flach, im Speicher 51–272 MB" in README und Vignette entspricht `stream_hold_mb` (Größe eines Batches), die CSV zeigt 49,8–312,2 MB.

## 5. Heruntergestufte Befunde (nach der Gegenprüfung minor)

`c1-spec-18` (SAS-Datums-Regex prüft nur die Form; so in REQ-VAL-031 spezifiziert, `DTA_YAML_GUIDE.md:391` verspricht aber mehr), `c2-data-32` (Dateianzahl als Summe über Einträge, so in REQ-VAL-036 spezifiziert), `c3-stream-32` (Arrow-Cast ohne Bit-Exaktheits-Gate, Kollisionspaar nicht gezeigt), `c4-tmpl-export-02/-03/-39` (der Auslöser "unerwartete Regelform" von `-39` ist widerlegt, denn `lint_dta()` fängt das selbst; real bleibt, dass ein Abbruch in `lint_dta()` zu "keine Befunde" wird, z. B. durch einen ungültigen Dateinamen-Regex), `q1-02/-26/-27/-28`, `q2-02/-07`, `q3-19/-22/-34/-35/-60`.

## 6. Systemische Muster (Ursachen statt Einzelfälle)

1. **"Nicht geprüft" fällt in "ok" zusammen.** Leere Tabelle, bares Backend, Schema-Kompilierfehler, `dv_findings()`-Fehler, `lint_dta()`-Abbruch in `validate_template()`, unbehandelter Rest in der Status-Map, Verdikt `PASS (PARTIAL)`/`PASS WITH NOT-EXECUTED TESTS` im CI-Gate, `required: true`-Slot-Fehler. Der Grundfehler ist derselbe: Verdikte werden aus dem Fehlen von Befunden abgeleitet, nicht aus dem Nachweis, dass geprüft wurde. Ein eigener Zustand "unbekannt/nicht geprüft" mit Scheitern bis zur Gegenzeichnung wäre die systemische Lösung.
2. **Tests prüfen den Aufbau, nicht das Produkt.** Akzeptierte Menge gleich Ausdruck, Lauf mit sich selbst verglichen, Skip als einziger Fehlerpfad, `testServer`-No-ops (`update*Input`, `showModal`, `sendCustomMessage`, `outputOptions`, Notifications), Download-Handler nie aufgerufen, Outputs nie gerendert. Die Suiten sind grün, weil sie das Verhalten kaum binden.
3. **Kopien der Engine in der Shiny-App driften ab.** Status-Map, Dry-Builds ohne Party-Auswahl, die `%||%`-Variante mit zero-length-als-fehlend (`R/` nutzt rlang-`%||%`, nur `utils_dta.R:35` ist die andere), Regeleditor-Parser. Doppelte Logik an zwei Stellen führte jedes Mal zu einem Defekt.
4. **Die Qualifikationssuite bricht ihre eigene Leitregel** (Erwartungswerte nie aus der Software) und ihre Meta-Checks werden selbst kaum getestet; die Traceability-Regel widerspricht der veröffentlichten Doku.
5. **Doku aus der falschen Methode/Quelle** (generische Rd-Seiten), veraltete Aussagen (JSON-Schema, `ok`-Spalte, `max_errors`).
6. **Kompatibilitätsschicht vor 1.0.0**, die nur Pflege kostet.

## 7. Rohbefunde nach Bereich

| Bereich | Befunde | blocker / major | Bemerkung |
| --- | --- | --- | --- |
| c1 Spezifikationsmodell | 62 | 0 / 6 | Alle 22 Dateien gelesen |
| c2 Datenschicht, Engine | 51 | 0 / 6 | Alle 24 Dateien gelesen |
| c3 Streaming, Reporting | 34 | 0 / 6 | Alle 7 Dateien gelesen |
| c4 Templates, Export, Runner | 57 | 1 / 7 | Alle 17 Dateien gelesen |
| s1 `app.R` | 29 | 3 / 6 | 11.206 Zeilen vollständig |
| s2 Shiny-Hilfsmodule | 53 | 1 / 6 | Alle 18 Dateien gelesen |
| t-core-1 | 39 | 1 / 0 | Alle Dateien gelesen |
| t-core-2 | 41 | 0 / 1 | Alle 36 Dateien gelesen |
| t-core-3 | 43 | 0 / 2 | Alle Dateien gelesen |
| t-shiny-1 | 42 | 1 / 1 | Alle 28 Dateien gelesen |
| t-shiny-2 | 49 | 1 / 1 | Alle 29 Dateien gelesen |
| q1 Qualifikations-Infrastruktur | 37 | 0 / 10 | `fx/t` nicht einzeln gelesen |
| q2 Qualifikationstests A | 48 | 1 / 11 | Alle 26 Dateien gelesen |
| q3 Qualifikationstests B | 74 | 2 / 31 | Alle 41 Dateien gelesen |
| d1 Nutzerdoku | 39 | 0 / 3 | Teile nur mechanisch (s. Abschnitt 1) |
| d2 Metadaten, CI | 37 | 0 / 2 | |
| **Summe** | **735** | **11 / 99** | |

Kategorien insgesamt: test-weak 224, doc-wrong 123, bug 77, inconsistency 66, dead-code 63, code-cheat 53, design 33, test-cheat 32, logic-error 28, doc-missing 16, security 13, perf 7.

## 8. Vorschlag zur Reihenfolge

1. **Silent-Datenverlust im Regel-Editor und Status-Map** (`s1-app-01/-02/-03/-28`, `s2-shiny-r-03`, Vokabular-"None"): Der Nutzer speichert oder bescheinigt etwas anderes, als er sieht.
2. **Verdikt-Semantik vereinheitlichen** (Muster 1): bares Backend, Schema-Kompilierfehler, leere Tabelle, `lint_dta()`-Abbruch, `dv_findings()`.
3. **Regelsemantik**: `pattern` auf numerischen Spalten (`c2-25`), literale Mehrnamen-Einträge (`c2-44`), Empty-String-Regel (`q3-27`: entweder umsetzen oder als Abweichung registrieren), Gruppenbedingungs-Schlüssel (`c1-49`).
4. **Qualifikationssuite**: Traceability-Regel mit Doku in Einklang bringen und per Test pinnen, die Meta-Checks mit Fehlschlagpfad testen, die nicht-scheiternden Fälle (Abschnitt 4.5) durch echte Orakel ersetzen, CI-Gate für `PASS WITH NOT-EXECUTED TESTS`/`PARTIAL` entscheiden.
5. **testthat**: Verschachtelten Test in `test-DTAFile.R` auflösen, Download-Handler- und Render-Tests schreiben, `testServer`-No-ops dort mocken oder die Prüfung in einen echten Browser-Test verlegen.
6. **Doku**: generierte Hilfe der Generics, `max_errors`, `results()`-`ok`, JSON-Schema-Aussagen, CHANGELOG-Duplikat, Zahlen zum Streaming-Speicher.
7. **Kompatibilitätsschicht und Altlasten löschen** (Pre-1.0).
8. **S7:** Klären, ob aktuelles S7 `new_object(.parent = ...)` bricht (der Minimaltest steht oben), und `DESCRIPTION`, CI und Installationsanleitung konsistent machen.

## 9. Dateien

Alle Belege liegen in diesem Ordner:

- `REVIEW-REPORT.md` — dieser Bericht
- `REVIEW-APPENDIX-all-findings.md` — alle 735 Rohbefunde als Tabelle
- `findings/<bereich>.md` — Befunde mit Beleg, Fehlerszenario und Konfidenz je Bereich
- `verify/results-V1.md` bis `results-V6.md` — Gegenprüfung der 110 Befunde mit Schwere blocker/major
- `verified-list.txt` — die 110 gegengelesenen Befunde mit Verdikt

Nicht archiviert: die Arbeitsanweisungen an die Leser, die Laufprotokolle des Runners und der geprüfte Stand als Klartext.
