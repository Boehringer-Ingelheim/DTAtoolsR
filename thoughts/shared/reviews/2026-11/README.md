# Code-Review 2026-11: Belege

Ablage der Belege zum vollständigen Code-Review vom 21.–23.11.2026 (735 Rohbefunde; die 110 mit Schwere blocker/major wurden gegengelesen). Die Befund-IDs in den Plänen (`PLAN-review-blockers-2026-11.md`, `PLAN-review-remainder-2026-11.md`) verweisen auf diese Dateien.

- `REVIEW-REPORT.md`: Gesamtbericht mit Methode, Grenzen und Reihenfolgevorschlag. Der Bericht beschreibt den geprüften Stand `68408c44`; Zeilennummern gelten dafür, nicht für den heutigen `dev`.
- `REVIEW-APPENDIX-all-findings.md`: alle Rohbefunde als Tabelle (ID, Schwere, Kategorie, Fundstelle, Kurzbeschreibung).
- `findings/`: vollständige Befunde je Bereich mit Beleg, Fehlerszenario und Konfidenz.
- `verify/`: Gegenprüfung der blocker/major-Befunde durch einen zweiten Leser mit dem Auftrag, sie zu widerlegen; `verified-list.txt` fasst die Verdikte zusammen.

**Bereinigt.** Die Repository-Regel verbietet jede Erwähnung von Assistenten. Deshalb sind in diesen Dateien die Namen der Werkzeuge und Modelle durch `[assistant-tool]` ersetzt, auch dort, wo ein Befund einen Repository-Pfad oder einen Dateinamen wörtlich zitiert (etwa das Konfigurationsverzeichnis eines Werkzeugs). Wer einen Pfad suchen muss, liest ihn aus dem Repository, nicht aus dem Zitat. Die unbereinigte Sicherungskopie liegt außerhalb des Repositorys.

Die Dateien sind ein Messprotokoll, kein lebendes Dokument: Befunde, die inzwischen behoben sind, bleiben stehen. Der Stand der Behebung steht in den Plänen.
