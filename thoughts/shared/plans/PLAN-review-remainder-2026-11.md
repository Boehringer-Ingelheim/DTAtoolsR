# Behebungsplan: die übrigen Befunde des Code-Reviews

Grundlage: der Gesamt-Review vom 21.–23.11.2026 (735 Rohbefunde; die 110 mit Schwere blocker/major wurden von einem zweiten Leser gegengelesen, 0 widerlegt). Die IDs wie `c3-stream-02` verweisen auf dessen Befunddateien, die nicht im Repository liegen (siehe Entscheidung Q8). Zeilennummern stammen aus dem geprüften Stand (`dev` @ `ec76a8f5` + File-drop-Fix); `dev` hat sich seither bewegt. Die Stellen sind über die Symbolnamen wiederzufinden. Der Plan ist die Fortsetzung von `PLAN-review-blockers-2026-11.md`.

Es gelten die Projektregeln (`CLAUDE.md`): Arbeit geht über Pull Requests gegen `dev`, kein Versions-Bump, `CHANGELOG.md` nur unter `## [Unreleased]`, keine Rückwärtskompatibilität vor 1.0.0, keine Erwähnung von Werkzeugen oder Assistenten in Commits, PR-Texten oder Dateien.

## 0. Stand und Umfang

**Erledigt (alle in `dev` gemergt, CI grün):** die acht Blocker und ihre Nachbarn, zusammen 18 der 110 gegengelesenen Befunde.

| PR | Befunde |
| --- | --- |
| #229 | `s1-app-28`, `s2-shiny-r-03` (Status-Map) |
| #230 | `c1-spec-31`, `c2-data-50`, `d1-userdocs-35`, `d2-meta-ci-01` (S7 >= 1.0.0, `DESCRIPTION` und CI konsistent; geprüft: `DESCRIPTION` und die `extra-packages`-Zeilen der Workflows, nicht die Installationsanleitungen) |
| #231 | `t-shiny-1-14`, `s2-shiny-r-20`, `-21` (Vokabular "None") |
| #235 | `q3-qual-tests-b-27`, `c2-data-25` (`pattern`) |
| #236 | `q3-qual-tests-b-17` (OQ-ROBUST-012) |
| #237 | `s1-app-01`, `-02`, `-03`, `-04` (Regel-Editor) |
| #238 | `t-shiny-2-10`, `q3-qual-tests-b-18`; `q3-qual-tests-b-21` nur teilweise (Klasse `dta_encoding_invalid`) |

**Offen:**

- **75 bestätigte Majors** (davon 74 verschiedene Themen: `c4-tmpl-export-01` und `q1-qual-infra-01` sind derselbe Befund).
- **17 bei der Gegenprüfung auf minor herabgestufte Befunde** (gegengelesen, aber klein): `c1-spec-18`, `c2-data-32`, `c3-stream-32`, `c4-tmpl-export-02`, `-03`, `-39`, `q1-qual-infra-02`, `-26`, `-27`, `-28`, `q2-qual-tests-a-02`, `-07`, `q3-qual-tests-b-19`, `-22`, `-34`, `-35`, `-60`.
- **625 minor/nit-Befunde, nicht gegengelesen.** Aufschlüsselung: test-weak 188, doc-wrong 118, bug 59, inconsistency 65, dead-code 62, code-cheat 43, design 30, test-cheat 15, doc-missing 15, logic-error 14, security 10, perf 6.
- **Reste aus PR #238:** Ablehnung von Binärdateien und abgeschnittenen Archiven ohne eigene Klasse (`q3-qual-tests-b-21`); `tables()`, die Exporte und der Report-Writer treffen bei einer lazy Tabelle auf Arrows eigenen Fehler; der `check()`-Scan überspringt nicht deklarierte Spalten; ein nicht registrierter Fehler in `dta_delim_header_names()` (gerissene Zeile mit ungültigen Bytes im ersten Block, berührt REQ-ROBUST-015); `OQ-ROBUST-015` unberührt.
- **Offene Frage aus #235:** Soll die Spalten-`pattern`-Prüfung `""` ebenfalls ablehnen (LIM-036; heute unverändert)?

## 1. Entscheidungen, die ich von dir brauche

**Q1: Was gilt, wenn eine Prüfung wegen eines Spezifikationsfehlers nicht laufen kann?** Heute: eine Warnung, die Wertprüfungen der Spalte entfallen, die Tabelle ist `ok = TRUE` (`c1-spec-16` bares `type: SAS`, `t-core-2-36` Schema-Kompilierfehler; der Test pinnt die Warnung, nie das Verdikt). Derselbe Mechanismus: `s1-app-07` (`dv_findings()` macht jeden Fehler zu "Packaging conforms").
- *Empfehlung (a):* Eine Prüfung, die wegen der Spezifikation nicht laufen kann, ist ein Fehler: `ok = FALSE` plus ein Befund mit eigener Klasse (`dta_check_not_run`), und die App zeigt "nicht prüfbar" statt "bestanden". Der Spezifikationsautor muss es beheben; das ist dasselbe Prinzip wie bei den Matrix-Fehlplatzierungen.
- Kopfzeilen-Tabelle ohne Zeilen (`c2-data-28`, Designfrage): *Empfehlung:* `ok = TRUE` bleibt (der Code nennt es bewusst "ein legitimes Ergebnis"), aber die Zusammenfassung sagt "0 Zeilen, Wertprüfungen nicht ausgeführt" statt "passed validation", und die Einschränkung wird als LIM registriert. Alternative: `ok = NA` wie bei null Spalten.

**Q2: Wann gilt eine Anforderung als "verified"?** Heute genügt **ein** bestandener Fall, auch wenn die anderen übersprungen wurden (`q1-qual-infra-01` = `c4-tmpl-export-01`). README, `maintaining.md` und Validierungsplan versprechen das Gegenteil; der Plan (`PLAN-qualification-suite.md:200`) hatte "≥ 1 bestanden" absichtlich so definiert.
- *Empfehlung (a):* Code folgt der veröffentlichten Doku. Eine Anforderung ist nur dann `verified`, wenn alle ihre Fälle ausgeführt wurden und bestanden; sonst `partially verified` (mindestens einer lief). Eine auditfähige Suite darf nicht auf übersprungenen Fällen zertifizieren. Der Validierungsplan wird entsprechend geändert.
- (b): Die Doku folgt dem Code. Billiger, aber README und `maintaining.md` müssen dann das Gegenteil dessen sagen, was sie heute sagen.

**Q3: Soll das Qualifikations-CI-Gate bei `PASS WITH NOT-EXECUTED TESTS` und `PASS (PARTIAL)` fehlschlagen?** Heute schlägt es nur bei `^FAIL` an (`d2-meta-ci-07`), und der Workflow installiert kein TinyTeX.
- *Empfehlung (a):* Ja. Der Workflow installiert pandoc und TinyTeX, damit die umgebungsbedingten Skips verschwinden; jeder verbleibende Skip steht in einer Allow-Liste im Repository. Kosten: mehr CI-Minuten.
- (b): Nein, aber das Verdikt erscheint als Warnung im Job-Log.

**Q4: Die Kompatibilitätsschicht vor 1.0.0 löschen?** `upgrade_dta()`, `dta_is_stale()` an 14 Einstiegsstellen, tolerante Accessoren, Artefakt-Migration, `dataset_only`, der `prov$selections`-Replay-Zweig und alte Template-Listing-Helfer in der App (`t-core-3-17`, `c2-data-06`, `-37`). Sie ist kein toter Code, sondern läuft an jedem Einstiegspunkt und ist qualifiziert (REQ-CONT-029, REQ-MATRIX-059, OQ-CONT-047, OQ-MATRIX-Fälle).
- *Empfehlung (a):* Ja, in einem eigenen PR nach einer Bestandsaufnahme jeder Fundstelle (Aufrufer prüfen, nicht aus der Befundliste löschen). Die Anforderungen und Fälle werden zurückgezogen, nicht stillschweigend gelöscht. **Vorbehalt:** `dta_stored_class()` und `dta_class_storage()` (aus der S7-1.0.0-Anpassung, `R/containerAssignments.R`) und alles, was Objekte über Versionsgrenzen speichert (`saveRDS()`, Shiny-Wiederherstellung), bleibt, sofern es tatsächlich genutzt wird. Das klärt die Bestandsaufnahme.
- (b): Behalten, bis 1.0.0 ansteht.

**Q5: `type: file`-Dataset mit `columns:` oder `rules:`.** Heute werden beide still verworfen, bei Matrix-Datasets sind die spiegelbildlichen Fehlplatzierungen harte Fehler (`c1-spec-17`). *Empfehlung:* harter Fehler (kein Kompatibilitätsgrund, siehe Regel).

**Q6: Wörtlicher Dateieintrag mit mehreren Namen** (`filename: [report.pdf, listing.pdf]`, `c2-data-44`). Der Code ist in sich widersprüchlich: `matches_filename()` und die erzwungene Anzahl 1 sagen "eine Datei unter irgendeinem dieser Namen", der Kommentar im Datei-Dataset verspricht ein Urteil je Name.
- *Empfehlung (a):* Alternativen. Eine Datei unter einem der Namen; die Anzahl regeln `min`/`max_number_of_files`. Wer zwei Dateien verlangt, schreibt zwei Einträge. Der Datei-Pfad muss dann dieselbe Anzahlprüfung wie der tabellarische bekommen (beide Namen geliefert bei Anzahl 1 ist heute ein stiller Pass), und die falschen Kommentare werden korrigiert.
- (b): Je Name ein Urteil; das ändert die Anzahlsemantik aller Handler.

**Q7: PDF-Export fällt still von Word/LibreOffice auf TinyTeX/pandoc zurück** und verliert Kopf- und Fußzeile (`c4-tmpl-export-16`). *Empfehlung (a):* Der Rückfall bleibt, erzeugt aber immer eine Warnung mit eigener Klasse, die das fehlgeschlagene Backend und die Folge nennt. Viele Rechner haben legitim nur TinyTeX, ein Fehler wäre zu hart. (b): Fehler, außer das Backend wird ausdrücklich gewählt.

**Q8: Review-Belege ins Repository?** Die Befunddateien liegen im temporären Arbeitsverzeichnis des Reviews, das ein Neustart oder eine Bereinigung löschen kann (eine Sicherungskopie außerhalb des Repositorys ist angelegt). *Empfehlung (a):* Bericht, Anhang und Befunddateien nach `thoughts/shared/reviews/2026-11/` kopieren, **vorher bereinigen**: Der Bericht nennt im Abschnitt "Reviewer" die verwendeten Modelle und Agenten, was die Repository-Regel gegen jede Erwähnung von Assistenten verbietet. Ohne Archiv bleibt dieser Plan die einzige Quelle für die IDs der Majors. (b): nur diesen Plan.

**Q9: Zweitlesen der 141 riskantesten ungeprüften Minor/Nit-Befunde** (bug 59, logic-error 14, security 10, code-cheat 43, test-cheat 15), mit demselben Verfahren wie die erste Gegenprüfung (sechs unabhängige Leser mit dem Auftrag, zu widerlegen). *Empfehlung:* ja, vor Schritt 6 der Reihenfolge (Abschnitt 5); bestätigte Befunde wandern in die passende Welle. Die übrigen 484 (test-weak, doc, dead-code, inconsistency, design, perf) werden nicht einzeln gelesen (Abschnitt 4).

**Q10: Spalten-`pattern` und `""` (LIM-036).** *Empfehlung:* unverändert lassen, wie in #235 gemergt. Wenn du `""` auch in der Spaltenprüfung ablehnen willst, ist das ein kleiner Nachtrag zu Welle 2.

## 2. Schritt 0: Proben vor den PRs (je etwa 15–30 Minuten, kein PR)

Jede Probe entscheidet, ob ein Befund echt ist oder wie groß der PR wird. Wie bei `t-shiny-2-10` gilt: erst messen, dann planen.

| Probe | Befund | Frage | Wirkung |
| --- | --- | --- | --- |
| P1 | `s2-shiny-r-45` | `as.character(shiny::selectizeInput("x", NULL, choices = NULL, options = list(options = list(list(value = "a", label = "</script><b>")))))`: kommt `</script><b>` unmaskiert heraus? Dann ist ein Template-Label ein XSS-Weg. Zweite Probe für den zweiten Weg desselben Befunds: eine Option-**ID** mit `</script>` läuft über `jsonlite::toJSON` in einen `I()`-`onChange`-Handler (`template_ui.R`, `with_suggestions`); die erzeugte Seite lesen, ob das Zeichenpaar maskiert ist. | Bestätigt: wird zum ersten PR überhaupt (Sicherheit), vor allen Wellen. Nicht bestätigt: Befund schließen. |
| P2 | `c3-stream-01`, `-02` | Lazy CSV mit deklariert numerischer Spalte (`"30","abc","70"`, Bereich 18–65): liefern `check()` und `inspect()` dieselben Zeilen? Und `summarised_error` bei mehr als `max_errors` Fehlern? | Größe der Welle 4. |
| P3 | `q1-qual-infra-03` | `R CMD INSTALL --build .` und `nrow(vignette(package="DTAtools")$results)` im Installationslauf: listet die CI-Installation Vignetten? | Entscheidet, ob IQ-ENV-019 in der CI je ausgeführt wird. |
| P4 | `q1-qual-infra-08` | Skalen-Generator bei Zeile 10.000.000: schreibt er `S10000000` (acht statt neun Stellen) und liegt das Orakel um zwei Fehler daneben? Per Rechnen aus dem Generator, nicht per Lauf der Stufe `full`. | Ist die Stufe `full` kaputt, muss das vor jedem Release-Lauf in Welle 8. |
| P5 | `s1-app-05` | In einer laufenden App eine benannte Bedingung umbenennen und `input$gconstr_left_1` lesen. | Nur ein Beobachtungsbefund, der Auslöser ist bislang gefolgert. |
| P6 | `s1-app-07` | Welche Eingabe lässt `container_findings()` werfen (veraltetes Objekt, `S7::validate`)? | Wird durch Q4 teils gegenstandslos. |

## 3. Wellen und PRs

Namenskonvention: **W1 … W13** (Wellen), damit sie nicht mit den PR-Buchstaben A–F der Blocker kollidieren. Reihenfolge, Abhängigkeiten und Parallelität stehen in Abschnitt 5.

| Welle | Thema | Befunde | Art | Größe | Braucht |
| --- | --- | --- | --- | --- | --- |
| W1 | Urteil ehrlich machen | `c1-spec-16`, `t-core-2-36`, `c2-data-28`, `s1-app-07`, `c4-tmpl-export-39` | Produkt + Tests | M | Q1 |
| W2 | Spezifikation streng lesen | `c1-spec-17`, `c1-spec-49`, `t-core-3-37` | Produkt + Tests | M | Q5 |
| W3 | Dateieinträge und Zahlen-Writer | `c2-data-44`, `c1-spec-36`, `c2-data-32` | Produkt + Tests | S–M | Q6 |
| W4 | Streaming und `inspect()` | `c3-stream-01`, `-02`, `-12`, `-13`, `-25`, `c3-stream-32` | Produkt + Tests | L | P2 |
| W5 | Export | `c4-tmpl-export-16`, `-22`, `-29`, `-02`, `-03` | Produkt + Tests | M | Q7 |
| W6 | Template-Engine der App | `c4-tmpl-export-45`, `s2-shiny-r-19`, `-25`, `-38` | Produkt + Tests | M–L | – |
| W7 | App: Fehler nicht verschlucken | `s1-app-05`, `-06`, `-13`, `s2-shiny-r-45` | App + Tests | M | P1, P5 |
| W8 | Qualifikation: Urteil und Gate | `q1-qual-infra-01` (= `c4-tmpl-export-01`), `-03`, `-08`, `-19`, `q2-qual-tests-a-01`, `-08`, `d2-meta-ci-07`, `q1-qual-infra-02`, `-26`, `-27`, `-28` | Suite + Doku | M | Q2, Q3, P3, P4 |
| W9 | Qualifikation: Meta-Checks prüfbar | `q1-qual-infra-16`, `-29` | Suite | L | W8 |
| W10 | Qualifikation: Fälle, die nichts prüfen | siehe 3.10 (mit `q2-qual-tests-a-02`, `-07`, `q3-qual-tests-b-19`, `-22`, `-34`, `-35`, `-60`) | Suite | L (4–5 PRs) | W8 |
| W11 | testthat: Tests, die nichts prüfen | `t-shiny-1-01`, `t-shiny-2-14`, `t-core-1-26`, verschachtelter `test_that`, Test mit `git show f6189bf` | Tests | M | – |
| W12 | Kompatibilitätsschicht löschen | `t-core-3-17`, `c2-data-06`, `c2-data-37` und Verwandte | Löschen + Suite | L | Q4 |
| W13 | Dokumentation | `c2-data-01`, `d1-userdocs-20`, `-26`, `d2-meta-ci-24`, `c1-spec-18`, CHANGELOG-Duplikat | Doku | M | W4 |

### 3.1 W1: Urteil ehrlich machen (Q1)

**Ursache.** Verdikte werden aus dem Fehlen von Befunden abgeleitet, nicht aus dem Nachweis, dass geprüft wurde (systemisches Muster 1 des Berichts). `dta_columnspec_declared_columns()` lässt eine Spalte ohne Schema aus den Nennern der Wertprüfungen fallen; `columnspec_valid = !has_columnspec_errors` sieht den Ausfall nie. Dasselbe in `dv_findings()` und in dessen Zwilling im Berichts-Builder (`utils_dta.R`, `tryCatch(..., error = NULL)`).

**Änderung.**
1. `R/columnSpecChecks.R`, `R/validationFunctions.R`: Eine Spalte, deren Schema nicht gebaut werden kann, erzeugt einen Fehlerbefund (Klasse `dta_check_not_run`) und macht `columnspec_valid` zu `FALSE`; die Warnung `dta_columnspec_schema_failed` bleibt als Hinweis auf die Ursache. Ein bares `type: SAS` ohne Typ wird zusätzlich schon in `lint_dta()` und im Spezifikationsvalidator gemeldet (heute prüft keiner von beiden den fehlenden Typ).
2. App: `dv_findings()` und der Berichts-Builder fangen den Fehler nicht mehr zu "keine Befunde"; ein Fehler wird als eigener Zustand "nicht prüfbar" angezeigt, nie als "Packaging conforms" oder grüner Zertifizierungsknopf. Der Auslöser (P6) ist unbekannt; der Test erzwingt ihn über einen Mock von `container_findings()`.
3. `lint_dta()`-Abbruch in `validate_template()` (`c4-tmpl-export-39`, minor): wird zum Befund, nicht zu "keine Befunde".
4. `c2-data-28` je nach Q1: Wortlaut der Zusammenfassung ändern, LIM registrieren.

**Tests (zuerst rot).** Jeder Test prüft das **Verdikt**, nicht die Warnung: Tabelle mit schlechten Werten in einer Spalte ohne Schema ⇒ `ok == FALSE`. Löschen von `cli_warn()` darf den Test nicht retten. Vorsicht: Tests mit dem Kommentar `KNOWN DEFECT, pinned rather than endorsed` suchen (`grep`); sie sollen kippen und nennen die Assertion, auf die umzustellen ist.

**Qualifikation.** Neue Anforderung in `REQ-VAL` ("eine nicht ausführbare Prüfung ist nie ein Pass") mit OQ-Fall, Erwartung von Hand; `min_tests` anheben; Manifest synchronisieren.

### 3.2 W2: Spezifikation streng lesen (Q5)

- `c1-spec-17`: `type: file` mit `columns:`/`rules:` ⇒ `cli_abort()`, gleiche Form wie die Matrix-Fehlplatzierung.
- `c1-spec-49`: Schlüssel von Gruppenbedingungen werden geprüft (unbekannter Schlüssel ⇒ Fehler, kein stilles `"any"`); `cst$left` darf nicht per Partial-Matching `left_scope` treffen (`[[`-Zugriffe mit `exact = TRUE`).
- `t-core-3-37`: Spaltenmapping aus YAML mit unbekannten Schlüsseln wird vor `do.call(DTAColumnSpec, raw)` abgelehnt, mit Liste der erlaubten Schlüssel.
- Tests je Fall mit einem Tippfehler als Eingabe und `class =` im `expect_error()`.

### 3.3 W3: Dateieinträge und Zahlen-Writer (Q6)

- `c2-data-44`: Umsetzung der Entscheidung. Bei (a): Anzahlprüfung im Datei-Pfad, Tests mit `pattern = FALSE` (heute laufen alle mit `pattern = TRUE`; der Kommentar in `test-DTAFile.R:1388` behauptet einen Zweig, den die Tests nie erreichen), Kommentare und `DTAFileAny`-Doku korrigieren.
- `c1-spec-36`: `write_columns_to_json()`, `_yaml()` und `as_json_schema()` schreiben ohne Rundung (`digits = NA` in jsonlite, volle Genauigkeit in yaml). Test: Wert `0.123456789012345` überlebt Schreiben und Lesen **bitgleich**; erwartet wird der Literalwert, nicht der Rückgabewert der Funktion. Die Rundung durch die Bibliotheken ist aus deren Dokumentation belegt, nicht ausgeführt: der rote Lauf kommt zuerst.

### 3.4 W4: Streaming und `inspect()` (P2 zuerst)

- `c3-stream-02`: `inspect()` und `explain_messages()` bewerten ein gestreamtes Dataset auf ungetyptem Rohtext neu und widersprechen `check()`. Ziel: dieselbe Typisierung (`dta_coerce_table_to_specs`) wie in der Prüfung, oder die Zeilen aus den vom Streaming-Lauf gespeicherten Fehlern lesen. Das Orakel ist der eager-Pfad desselben Datasets: Tabelle im Speicher und gestreamt müssen dieselben Zeilen liefern (Differentialtest über eine kleine Fixture-Menge, Erwartung von Hand abgeleitet, nicht aus `inspect()`).
- `c3-stream-01`: `summarised_error` aus dem gekappten Kopf; Fix: Zusammenfassung vor dem Kappen oder Kennzeichnung der Kappung.
- `c3-stream-12`: `inspect()` bricht bei einer nicht auswertbaren IF/THEN-Regel ab ⇒ Befund statt Abbruch.
- `c3-stream-13` (Performance, ungemessen): erst messen (`bench`-Lauf mit N Meldungen), nur bei messbarem Gewinn ändern.
- `c3-stream-25`: "Non-numeric values"-Hinweise in `rulePhrasing` sind für deklariert numerische Spalten falsch; Hinweis abhängig vom deklarierten Typ.
- Reste aus #238 gehören hierher: Klasse für Binär-/Archiv-Ablehnung (`q3-qual-tests-b-21`), `tables()`/Exporte/Report-Writer auf lazy Tabellen, nicht deklarierte Spalten im `check()`-Scan, Fehler in `dta_delim_header_names()` (als Abweichung zu REQ-ROBUST-015 registrieren oder beheben).

### 3.5 W5: Export (Q7)

- `c4-tmpl-export-22`: offene maximale Dateianzahl wird als "2 to 0" exportiert. `.file_specs_df()` und `.file_count_sentence()` behandeln `NULL` als unbegrenzt ("2 or more"), wie `print_info()`. Test mit offenem Maximum durch Word-/Markdown-Export, Text gelesen.
- `c4-tmpl-export-29`: nicht gerenderte Block-Platzhalter und kaputte `{#DATASETS}`-Marker erzeugen **immer** eine Warnung mit eigener Klasse; `quiet` dämpft nur Fortschrittsmeldungen. `write_dta(template=)` und die App dürfen `quiet = TRUE` weiter übergeben.
- `c4-tmpl-export-16`: Rückfall-Warnung aus Q7, auch in `write_dta()` und `write_dataset_metadata()`.
- Lesen und umsetzen: `c4-tmpl-export-02`, `-03` (minor, nicht im Bericht beschrieben).

### 3.6 W6: Template-Engine der App

- `c4-tmpl-export-45`: Das Guide-Beispiel baut für `required: true`-Party-Slots kein Datasets-Kapitel; `validate_template()` füllt den Slot und lässt das Template durch. Der Dry-Build des Guides bekommt dieselbe Party-Auswahl wie die Validierung (erstes geeignetes Profil). Der Kommentar `template_core.R:1021-1025` ("harmlos") wird korrigiert.
- `s2-shiny-r-38`: derselbe Mechanismus: Template-Historie und Rebase scheitern für `required: true`-Slots. **Eine** gemeinsame Hilfsfunktion für "Party-Auswahl für Trockenläufe" statt zwei Kopien (systemisches Muster 3).
- `s2-shiny-r-19`: Ein Kind-Template überschreibt über einen Vokabular-Slot die Werte einer versiegelten Spalte. Siegelprüfung auf `vocabulary_slots` ausdehnen; REQ-TMPL-021 listet bislang nur Optionen und Party-Slots und muss erweitert werden.
- `s2-shiny-r-25`: Rebase schreibt positionelle Metadatenschlüssel (`supplier.contacts.1.email`) als benannte Elemente. `list_set_path()` erkennt Ziffern-Schlüssel bei unbenannten Listen.

### 3.7 W7: App (P1, P5)

- `s2-shiny-r-45`: nur wenn P1 bestätigt. Dann ist das der dringendste PR des ganzen Plans: Termlabels, Beschreibungen und Option-IDs vor der Übergabe an `selectizeInput` maskieren (`</` ⇒ `<\/` im JSON), Test mit `</script><img onerror=...>` als Bibliotheksinhalt, im Browser geprüft.
- `s1-app-06`: Landing-Zone-Upload prüft Dateigröße und Endung vor `readLines()`/`read_yaml()`; die Fehlermeldung enthält nicht den gesamten Dateitext.
- `s1-app-13`: fehlgeschlagene Autosaves werden gemeldet (Benachrichtigung einmal pro Ausfall, `message` im Log); `store$commit()` nur nach erfolgreichem Schreiben (heute so), und das Schreiben läuft über eine temporäre Datei plus Umbenennen, damit ein Abbruch mitten im Schreiben den alten Stand nicht zerstört.
- `s1-app-05`: nur nach P5. Umbenennen einer benannten Bedingung aktualisiert die Verweise in allen Constraints; ein Constraint mit leerem Left/Right ist beim Speichern ein Fehler, kein stilles Verwerfen.

### 3.8 W8: Qualifikation, Urteil und Gate (Q2, Q3)

- `q1-qual-infra-01`: `qual_traceability()` (`R/qualification.R:690-758`) nach Q2. Neuer Status `partially verified`, Anzeige in Abschnitt 7 und `SUMMARY.txt`, README, `maintaining.md`, `validation-plan.md` und `PLAN-qualification-suite.md` angleichen. Test: eine Anforderung mit einem bestandenen und einem übersprungenen Fall wird **nicht** `verified`; diesen Test gibt es heute nicht. Meta-Check M10 zählt Skips als ausgeführt und muss mitziehen.
- `q1-qual-infra-19`: Das `affects:`-Feld einer Einschränkung erreicht die Matrix nie; REQ-VAL-026 und REQ-PERF-011 sind `verified`, obwohl ihre Fälle das Gegenteil festschreiben.
- `q1-qual-infra-03`, `q2-qual-tests-a-01`: IQ-ENV-016 und IQ-ENV-019 überspringen genau dann, wenn ihre Evidenzbasis fehlt. Ein fehlender Beleg ist ein Fehlschlag oder ein nicht ausgeführter Fall mit Folgen im Verdikt (je nach P3); die tautologische Restprüfung von IQ-ENV-019 wird ersetzt.
- `q1-qual-infra-08`: Skalen-Generator schreibt bei Zeile 10.000.000 `S10000000` (P4). **Vor jedem Release-Lauf der Stufe `full`** beheben; `full` ist der Standard von `run_qualification()`, die CI läuft ihn nie.
- `q2-qual-tests-a-08`: Das Verdikt wird nur mit Status `fail` geprüft, nie mit `error` oder `file_error`: Test je Status.
- `d2-meta-ci-07` und Q3: CI-Gate, Allow-Liste, pandoc und TinyTeX im Workflow.
- `q1-qual-infra-02`: `not_executed` für eine offene Abweichung erreicht das Verdikt nie; heute nicht auslösbar (beide offene Abweichungen sind unbedingt gebunden), aber billig zu härten.

### 3.9 W9: Qualifikation, Meta-Checks prüfbar

- `q1-qual-infra-16`: 13 von 14 Meta-Checks haben in keiner Suite einen Test, der sie **fehlschlagen** lässt. Jeder Meta-Check bekommt eine minimal verdorbene Fixture-Suite, auf der er rot wird; erwartet wird die Meldung des Checks, nicht dessen Aufruf.
- `q1-qual-infra-29`: Anforderungen wurden aus beobachtetem Verhalten abgeschrieben (Verstoß gegen die Leitregel "ein Erwartungswert stammt nie aus der Software"). Erst eine Liste der betroffenen Anforderungen aus dem Befund anlegen, dann je Anforderung die Quelle benennen (Spezifikation, Norm, Handrechnung) oder sie als Beobachtung kennzeichnen.
- Voraussetzung ist W8, weil beide die Verdikt-Logik berühren.

### 3.10 W10: Fälle, die nichts oder zu wenig prüfen

Vier bis fünf PRs, nach Bereich der Testdatei. Jeder Fall bekommt ein Orakel von Hand, eine Mutationsprobe (der Fall wird rot, wenn das Produkt falsch ist) und, wo möglich, ein Negativ-Kontrollpaar; das Vorbild ist `OQ-ROBUST-012` aus #236 und die Fälle aus #238.

- **Gruppe A, "kann nicht scheitern", zuerst:** `q2-qual-tests-a-13` (setzt ein Eingabefeld, das die App nicht hat; REQ-APP-017 ist faktisch unverifiziert), `-14` (die "nennt jedes fehlende Paket"-Prüfung wird vom Produktnamen erfüllt), `-15` (OQ-APP-002 ruft `run_dta_app()` nie auf), `q3-qual-tests-b-20`, `-28`, `-29`, `-36`, `-37`, `-42`, `-56`, `-59`, `-62`, `-66`.
- **Gruppe B, "deckt nur einen Teil ab":** `q2-qual-tests-a-16`, `-17`, `-18`, `-34`, `-35`; `q3-qual-tests-b-01`, `-02`, `-07`, `-13`, `-30`, `-38`, `-47`, `-48`, `-57`, `-63`, `-64`, `-65`, `-68`, `-69`. Bei einigen (`-63`, `-64`, `-65`) ist die Messung selbst das Problem (falsche Zeilennummern, Arrow-seitiges Wachstum nicht sichtbar, nur eine von sieben Messgrößen per Wert geprüft); dort zuerst die Messmethode klären, dann den Fall.
- `q3-qual-tests-b-21` steht nicht hier, sondern in W4: Es ist eine Produktänderung (Klasse für Binär- und Archiv-Ablehnung), kein reiner Testfall.
- Die herabgestuften `q3-qual-tests-b-19`, `-22`, `-34`, `-35` (Export-Fälle, OQ-EXPORT-032/-034 prüfen mehr als behauptet; offen bleiben Mutationen von `labels()`/`get_table()`) gehören in den Export-PR, `q3-qual-tests-b-60` in Gruppe A neben `-62`, `q2-qual-tests-a-02` und `-07` in den App-PR. Im Export-PR zusätzlich: OQ-EXPORT-010 meldet eine `cbind`-Recycling-Warnung unbekannten Ursprungs; erst klären, ob sie aus dem Produkt oder aus dem Fall kommt.
- Aufteilung: App (`q2-13` bis `-18`), Export (`q2-34`, `-35`, `q3-…` der Export-Dateien), Streaming und Leistung (`q3-56` bis `-69`), Rest. Genaue Zuordnung der q3-Fälle zu Dateien beim Planen des jeweiligen PRs aus `findings/q3-qual-tests-b.md` ablesen.
- Mindestens ein Fall je PR wird als Mutationsprobe im PR-Text protokolliert (roter Lauf).

### 3.11 W11: testthat, Tests, die nichts prüfen

- `t-shiny-1-01`: Der Test "HTML validation report download handler produces parseable HTML" ruft den Download-Handler nie auf; er ruft `write_validation_report()` selbst auf und durchsucht `app.R` als Text. Ersetzen durch einen Aufruf des echten Handlers (`output$dl_...` im `testServer`, Datei lesen).
- `t-shiny-2-14`: mindestens 16 der 70 Outputs der App rendert kein Test (u. a. `dl_yaml`, `dl_validation_summary`, das Add-Dataset-Panel); ein Absturz dort bliebe grün. Ein Test, der **jeden** Output der App einmal liest (Liste aus `app.R` gewonnen, nicht von Hand gepflegt, mit Allow-Liste für Outputs, die absichtlich nicht gerendert werden).
- Runner-Befund: In `tests/testthat/test-DTAFile.R` (Zeile 1224 bis etwa 1342) stecken 12 `test_that`-Blöcke ineinander (aktuell weiter vorhanden). Ein fehlschlagendes `expect_*` vor einem verschachtelten Block geht verloren. Auflösen in unabhängige Blöcke; danach eine Mutationsprobe: ein falscher Erwartungswert in Zeile 1228 macht die Suite rot.
- `t-core-1-26`: Paritätsfälle für Zeilen und Spalten im Kleinen sind vorhanden (herabgestuft); prüfen, was für den Großfall fehlt, und nur das ergänzen.
- Weitere Beobachtungen des Testlaufs (8 Skips, beobachtet, nicht gelesen): `test-DTADataSetMatrix.R:936` ruft `git show f6189bf:...` auf; die CI-Checkouts haben keine `fetch-depth`, der Test überspringt dort vermutlich immer (Überspringen in der CI nur vermutet, prüfen). Ein Test, der in der CI nie läuft, wird durch eine eingecheckte Fixture ersetzt, die den damaligen Stand enthält. Zwei Tests brauchen `DTATOOLS_LOCAL_LIBRARY`, das nichts in `.github/` setzt, die CI überspringt sie also ebenfalls. Das ist laut den Entwicklungsnotizen Absicht (die Variable darf nicht global gesetzt sein, weil `create_dta_from_template` sonst auf den Umgebungsindex zurückfällt und lokal scheitert, wo die CI grün bleibt); vor dem PR im Code bestätigen und entscheiden, ob eine kleine eingecheckte Bibliothek die Tests in der CI ausführbar macht. Die übrigen Skips (3× Symlinks, 1× Windows, 1× chromote-Start) sind plattformbedingt.
- Allgemeine Regel für die Welle: `testServer`-No-ops (`update*Input`, `showModal`, `sendCustomMessage`, `outputOptions`) werden gespielt oder gemockt, nie als Beobachtung benutzt. Wo ein No-op die Beobachtung verhindert, ein Browser-Test oder ein Spy wie in #238.

### 3.12 W12: Kompatibilitätsschicht löschen (Q4)

- **Bestandsaufnahme zuerst** (Ergebnis in den PR-Text): jede der 14 Einstiegsstellen von `dta_is_stale()`/`dta_upgrade()`, `dataset_only`, `prov$selections`, alte Template-Listing-Helfer; für jede: Aufrufer, Test, Anforderung. Ob irgendetwas Objekte über Versionsgrenzen speichert (`saveRDS()`, Shiny-Wiederherstellung), steht in diesem Ergebnis; was lebt, bleibt.
- Löschen, nicht umschreiben: Funktion, Export (`upgrade_dta`), Hilfeseite (`man/` neu erzeugen, nie von Hand), Tests (drei Dateien bauen ein altes Objekt nach: `test-upgrade.R`, `test-loadDelivery.R:1767-1808`, `test-shinyapp-containers.R:967-994`), Anforderungen und Fälle (REQ-CONT-029, REQ-MATRIX-059, OQ-CONT-047, OQ-MATRIX-yaml) werden **zurückgezogen** nach dem Verfahren in `maintaining.md`, `_api.yaml` bereinigt, Manifest neu erzeugt.
- Danach eine Aufrufersuche für die übrigen dead-code-Funde (62 minor/nit), in demselben oder einem Folge-PR; jeder Löschkandidat wird erst durch Lesen seiner Aufrufer bestätigt, nicht aus der Befundliste übernommen (R-Code hat kein Werkzeug, das ungenutzte Funktionen zuverlässig findet: S7-Methoden und Generics werden dynamisch aufgelöst).
- Dieser PR ist querschneidend (R/-Dateien an allen Einstiegen) und kollidiert mit fast allem; er läuft allein und möglichst früh, siehe Abschnitt 5.

### 3.13 W13: Dokumentation

- `c2-data-01`: Die generischen Rd-Seiten (`man/check.Rd`, `load_file.Rd`, `read_file_execution.Rd`) behalten nur einen `\item{...}` der Methoden. Ursache in der roxygen-Struktur (die Rd-Seite einer Generic wird aus den roxygen-Blöcken der Methoden zusammengesetzt; `@rdname` an einer S7-`method()` kann Exporte verlieren, und `@usage` muss zur Generic passen, nicht zur Methode), Fix in den roxygen-Blöcken, `man/` neu erzeugen und `NAMESPACE` auf Exportverluste prüfen. Der Runner fand keine roxygen-Drift, die Seiten sind also so erzeugt: prüfen, nicht raten.
- `d1-userdocs-20`: Vignette sagt, `max_errors` werde für Tabellen im Speicher ignoriert; tatsächlich kappt es die Details bei 10.000 (`validationFunctions.R:352-353`). Erst nach W4 (dort ändert sich die Kappung eventuell).
- `d1-userdocs-26`: `docs/index.html` dokumentiert eine Spalte `ok` in `results()`; es gibt sie nicht (16 Spalten ohne `ok`).
- `d2-meta-ci-24`: `bench_streaming.R` hält `tbl` und `typed` des Eager-Laufs während der Streaming-Messung; die Streaming-Messwerte sind damit ungültig. Skript korrigieren, Messung wiederholen, die Zahlen in README und Vignette ("~19 MB flach, im Speicher 51–272 MB" entspricht `stream_hold_mb`, der CSV-Wert ist 49,8–312,2 MB) aus der neuen Messung neu schreiben.
- Außerdem: die doppelt kopierte CHANGELOG-Passage, veraltete JSON-Schema-Aussagen und die 118 doc-wrong-Minor-Funde, soweit sie in Dateien liegen, die ohnehin geändert werden. Ein Sammel-PR für den Rest nach W12 (viele Fundstellen verschwinden mit der Kompatibilitätsschicht).

## 4. Die 625 ungeprüften Minor/Nit-Befunde

- **141 mit Fehlerpotenzial** (bug, logic-error, security, code-cheat, test-cheat): Zweitlesen laut Q9 vor Schritt 6 der Reihenfolge. Die Leser bekommen den Befund ohne mein Urteil und den Auftrag, ihn zu widerlegen; bestätigte Funde gehen in die passende Welle.
- **188 test-weak:** Regel für alle PRs: Wer eine Testdatei anfasst, prüft die test-weak-Befunde dieser Datei, behebt, was sich bestätigt, und nennt die verworfenen im PR-Text. Bei den gegengelesenen Befunden mussten 36 von 110 korrigiert werden; die ungeprüften sind also nicht blind anzuwenden. Kein eigener PR.
- **118 doc-wrong, 15 doc-missing:** W13 und die Dokumentationsarbeit der jeweiligen Wellen.
- **62 dead-code:** W12 und der Folge-PR (3.12).
- **65 inconsistency, 30 design, 6 perf:** Rückstand. Nach W13 erneut sichten; ein Plan dafür lohnt erst nach dem Zweitlesen, weil dort die Funde mit echtem Fehlerpotenzial herauskommen.

## 5. Reihenfolge und Parallelität

Gründe wie im Blocker-Plan: höchstens zwei Sessions gleichzeitig (das 5-Stunden-Limit hat Review und Umsetzung mehrfach unterbrochen; zuletzt haben die Sessions es von selbst überstanden), und jeder PR braucht eigene Test- und Review-Läufe. Wiederkehrende Rebase-Konflikte nur in `CHANGELOG.md` und `inst/qualification/baseline/inst-files.sha256`: CHANGELOG von der neueren Seite neu aufbauen (git kann Einträge in einen datierten Abschnitt verschieben), das Manifest nie von Hand mergen, sondern mit `sync_qualification_manifest.R` neu erzeugen.

| Schritt | PRs | Bemerkung |
| --- | --- | --- |
| 0 | Proben P1–P6 | zuerst; P1 kann W7 vorziehen |
| 1 | W12 allein (bei Q4 = ja) | querschneidend, löscht Tests und Anforderungen, macht alle späteren Wellen kleiner |
| 2 | W1 ∥ W2 | getrennte Dateien (`columnSpecChecks.R`/`validationFunctions.R`/App gegen die Spezifikationsklassen) |
| 3 | W3 ∥ W5 | getrennte Dateien |
| 4 | W4 ∥ W6 | getrennte Dateien (Streaming gegen Template-Engine) |
| 5 | W7 ∥ W8 | App gegen Qualifikationsinfrastruktur |
| 6 | Zweitlesen (Q9), danach W9 ∥ W11 | |
| 7 | W10 (4–5 PRs, je zwei parallel) | nach W8, damit die neuen Fälle unter der strengen Traceability laufen |
| 8 | W13, danach Sammel-PR Doku | nach W4 und W12 |

Ohne Q4 entfällt Schritt 1, die übrigen rücken auf.

**Je PR (wie im Blocker-Plan, Abschnitt 8):**
- Zuerst der rote Test mit von Hand abgeleiteter Erwartung; nie `results()` oder die geprüfte Funktion als Orakel.
- Mutationsprobe: Fix zurücknehmen oder Fehler einbauen, roter Lauf im PR-Text protokollieren.
- Style per `Rscript .github/scripts/style.R`; roxygenise mit der Version aus `Config/roxygen2/version`; Manifest per `sync_qualification_manifest.R` nach jeder `inst/`-Änderung.
- REQ/OQ nach `maintaining.md`; `min_tests` anheben; geänderte Anforderung heißt geänderte Anforderung, nicht nur geänderter Test.
- CHANGELOG unter `## [Unreleased]`; kein Versions-Bump; keine Erwähnung von Werkzeugen oder Assistenten in Commits, PR-Texten oder Dateien.
- Reviewer-Durchgang durch einen unabhängigen Leser, dem der Diff gegeben wird, nicht mein Urteil.
- Umgebung: `DESCRIPTION` verlangt S7 >= 1.0.0; eine ältere S7-Installation in der Benutzerbibliothek lässt das Paket nicht laden, dann S7 1.0.0 in eine eigene Bibliothek installieren und diese per `R_LIBS` an die Spitze stellen (die Benutzerbibliothek bleibt unberührt). Unter `load_all()` fehlen die Exporte `labels`, `names` und `print` absichtlich (`S7_on_build()`); das ist kein Fehler. In einem Git-Worktree zeigt `renv` auf eine leere Projektbibliothek: eine untracked `.Renviron` mit `RENV_CONFIG_AUTOLOADER_ENABLED=FALSE` im Worktree-Wurzelverzeichnis behebt das, `renv::restore()` nicht ausführen. `tests/testthat/_problems/` und `testthat-problems.rds` vor dem Staging löschen.
- CI per `gh run list --branch` beobachten, nicht per `gh pr checks`; Auto-fix an, Auto-merge aus; gemergt wird nur auf deine Anweisung nach grünen Workflows.

## 6. Risiken

- **Q1 ändert Verdikte.** Bestehende Fixtures und Tests, die eine Warnung pinnen, kippen; das ist gewollt, aber die Tests tragen den Hinweis `KNOWN DEFECT, pinned rather than endorsed` und müssen einzeln auf die richtige Assertion umgestellt werden. Eine Spezifikation, die heute durchläuft, kann danach scheitern: im CHANGELOG nennen.
- **Q2 stellt die Qualifikation ehrlicher dar.** Auf einer Maschine ohne shiny, pandoc oder TinyTeX erscheinen mehr Anforderungen als `partially verified`. Vorher prüfen, welche Pakete die CI-Läufer haben (Q3 hängt daran).
- **W12 löscht qualifizierte Anforderungen.** Die Meta-Checks der Suite erzwingen koordinierte Änderungen (Index, Manifest, API-Liste); ein PR, der nur den Code löscht, scheitert in der Qualifikation.
- **P1 kann alles verschieben.** Bestätigt die Probe das XSS, ist W7 vor allem anderen dran.
- **Archivierung (Q8).** Ohne Bereinigung verletzt der Bericht die Repository-Regel gegen Erwähnungen von Assistenten (er nennt die verwendeten Modelle). Bereinigen heißt: den Abschnitt "Reviewer" und die Methodennotizen entfernen, nicht den Inhalt der Befunde ändern.
- **Restrisiko aus dem Review selbst:** nicht oder nur oberflächlich gelesen wurden rund 200 YAML-Fixtures unter `inst/qualification/fx/`, etwa 50 Template-Fixture-Ordner, Binär-Fixtures (nur über Generatoren und Hashes), Teile von `private-templates.Rmd` und des Skeleton-READMEs sowie rund 470 `man/*.Rd`-Seiten ohne Gegenlesen. Keiner der Fehler dort ist bekannt, aber auch keiner ausgeschlossen.
- **Unvollständig bleibt, was der Review nie ausgeführt hat:** Aussagen über Laufzeitverhalten sind in den Befunden als `inferred` markiert. Wo ein PR davon abhängt, ist der rote Lauf Teil des PR (P2, P4, `c1-spec-36`).

## 7. Nicht in diesem Plan

- Die 65 inconsistency-, 30 design- und 6 perf-Befunde (Abschnitt 4).
- Ein Plan für die Stufe `full` der Qualifikation als Teil des Release-Prozesses (Läufe vor einem Release, `REQ-PERF-009`/`-010` sind in der Stufe `quick` nicht verifiziert). Hängt an `q1-qual-infra-08` (W8) und gehört in den Release-Prozess, nicht in diesen Plan.
- Alles, was Q4 = nein offen lässt.

## 8. Freigabe

Offen. Die Umsetzung beginnt nach deinen Antworten auf Q1–Q10 (Defaults siehe Empfehlungen) mit Schritt 0.
