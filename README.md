# ScrapeVattenfall

Scrape Vattenfall web page with daily electricity usage.

## Information on web page

One of the web pages on electricity usage available from the Vattenfall web site provides textual information on electricity usage and related monetary amounts for each day in a month. 

The page is [Overzicht &rarr; Verbruik](https://www.vattenfall.nl/service/mijn-vattenfall/#/vf/verbruik), select *Stroom*, *Dag*, month of interest, textual presentation (bullet list icon). The number of lines per day when no electricity is delivered to the network is lower than when it is ('Teruglevering'). See e.g. the entries for *4 mei* and *5 mei*.

![Part of Vattenfall web page with electricity usage for each day in a month](media/202305-Verbruik%20details-Vattenfall-w600-circle.png)

The page presentation is dynamically generated, saving it as HTML doesn't provide one with the desired information. Hence I simply copy-paste the whole page in a text file, which I then convert using the [script](script/scrape_electricity_per_day_vattenfall.py) to the [CSV representation](#the-csv-presentation) described further below.

Saved as text, this looks as follows, starting at the desired information, currently on line 27. The number of lines per day is 7 when no electricity is delivered to the network and it is 14 when it is ('Teruglevering').

<details>
<div style="text-size=.6em">

```
Mei 2023
De variabele leveringskosten hieronder zijn berekend zonder het prijsplafond. Wat het prijsplafond voor u betekent, ziet u terug in uw persoonlijke termijnbedrag advies.

Lees meer
1 mei € 2,37
Variabele kosten
7,162 kWh
€ 2,86
Vaste kosten
1 dg
€ -0,49
2 mei € 2,63
Variabele kosten
7,837 kWh
€ 3,12
Vaste kosten
1 dg
€ -0,49
3 mei € 2,10
Variabele kosten
6,505 kWh
€ 2,59
Vaste kosten
1 dg
€ -0,49
4 mei € 2,80
Variabele kosten
8,246 kWh
€ 3,29
Vaste kosten
1 dg
€ -0,49
5 mei € 0,16
Stroom
6,359 kWh
-
Teruglevering
-4,736 kWh
-
Netto verbruik
Variabele kosten
1,623 kWh
€ 0,65
Vaste kosten
1 dg
€ -0,49
6 mei € -1,62
Stroom
4,564 kWh
-
Teruglevering
-11,276 kWh
-
Netto verbruik
Variabele kosten
-6,712 kWh
€ -1,13
Vaste kosten
1 dg
€ -0,49
7 mei € -1,70
Stroom
```

</div>
</details>

The points of interest are then as follows, with the start of a day at index 0.
### A. Without Teruglevering

| Index |  Line contents   | {Datum}                |
| ----: | :--------------: | ---------------------- |
|     0 | 1 januari € 2,75 | [date] {Totale kosten} |
|     1 | Variabele kosten |                        |
|     2 |    8,838 kWh     | {Levering}             |
|     3 |      € 3,24      | {Variable Kosten}      |
|     4 |   Vaste kosten   |                        |
|     5 |       1 dg       |                        |
|     6 |     € -0,49      | {Vaste kosten}         |

### B. With Teruglevering

| Index |  Line contents   | {Datum}                |
| ----: | :--------------: | ---------------------- |
|     0 |  8 mei € -2,15   | [date] {Totale kosten} |
|     1 |      Stroom      |                        |
|     2 |    3,985 kWh     | {Levering}             |
|     3 |        -         |                        |
|     4 |  Teruglevering   |                        |
|     5 |   -13,862 kWh    | {Teruglevering}        |
|     6 |        -         |                        |
|     7 |  Netto verbruik  |                        |
|     8 | Variabele kosten |                        |
|     9 |    -9,877 kWh    | {Netto Verbruik}       |
|    10 |     € -1,66      | {Variabele kosten}     |
|    11 |   Vaste kosten   |                        |
|    12 |       1 dg       |                        |
|    13 |     € -0,49      | {Vaste kosten}         |


## The CSV presentation

```Text
Datum;Levering [Wh];Teruglevering [Wh];Netto Verbruik [Wh];Vaste Kosten;Variable Kosten;Totale kosten
01-01-23;8,838;0;8,838;-0.49;3.24;2.75
...
08-05-23;3,985;-13,862;-9,877;-0.49;-1,66;-2,15
```

## The script

Currently the [script](script/scrape_electricity_per_day_vattenfall.py) assumes the desired information starts on `line_month` (27). If needed, the scraping process can be made a bit more resilient. For now, let's hope the presentation does not often change.

The script's help screen:

```Console
usage: scrape_electricity_per_day_vattenfall.py [-h] [-v] [--csv-folder csv] [--output output] paths [paths ...]

Scrape given text file with Vattenfall electricity usage and create a csv file of it.

positional arguments:
  paths             file(s) with copy-pasted web page text (file, folder, wildcard)

optional arguments:
  -h, --help        show this help message and exit
  -v, --verbose     report file being processed (level 1), count (2), progress (3) (default: 0)
  --csv-folder csv  folder to write csv files to (default: None)
  --output output   output file in csv format (default: None)
```
