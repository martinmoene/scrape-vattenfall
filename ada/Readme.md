# Ada version of ScrapeVattenfall

Scrape Vattenfall web page with daily electricity usage.

## GNAT Toolset

The GNAT 2021 Community toolset is used, see the [Download GNAT Community Edition](https://www.adacore.com/download) AdaCore web page.

The program's help screen:

```Console
Usage: scrape_electricity_per_day_vattenfall [-h] [-v] [--csv-folder=csv] [--output=output] paths [paths ...]

Scrape given text file(s) with Vattenfall daily electricity usage and create file(s) in csv format. Single file output is to stdout default and can be directed to a file using option '--output'. When multiple files are specified, output is to a file of the same name with the extension replaced with '.csv'. Multiple file output can be directed to a folder using option '--csv-folder'.

positional arguments:
  paths             file(s) with copy-pasted web page text (file, folder, wildcard)

optional arguments:
  -h, --help        show this help message and exit
  -v, --verbose     report file being processed (level 1), count (2), progress (3) (default: 0)
  --csv-folder=csv  folder to write csv files to (default: None)
  --output=output   output file in csv format (default: None)
```
