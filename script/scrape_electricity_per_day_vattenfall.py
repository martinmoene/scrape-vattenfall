#!/usr/bin/env python
#
# Copyright 2023 by Martin Moene
#
# Distributed under the Boost Software License, Version 1.0.
# (See accompanying file LICENSE.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
#
# Scrape given text file with Vattenfall per-day electricity usage for its entries and create a csv file of it.
#
# Usage: script/scrape_electricity_per_day_vattenfall.py [-h] [-v] [options...] file.txt
#
# Dependencies:
# - Python standard library.
#
# Process:
# 1. ...
#
# Folders used/created:
# - ...
#
# Output:
# - Fields separated by ';'.
# - Energy in Wh without ',' or '.'.
# - Money in €, using '.' as separator, such as '-2.15' (optionally as '€ -2.15', see to_money()).
#

# TODO: Process multiple text files, reading an 'input' folder, writing to a 'csv' folder.
# TODO: Add option '-o output.csv' to direct output to an output file for a single text input file.

from __future__ import print_function

import re
import os
import sys
import argparse
import calendar

EXIT_SUCCESS = 0
EXIT_FAILURE = 1

LOG_FILENAME  = 1
LOG_PROCESSED = 2
LOG_PROGRESS  = 3

# Line information of text file copy-pasted from Vattenfal web page with a month's electricity usage per day:
line_month        = 27      # Note: per 11-May-2023
line_day_first    = 31      # Note: per 11-May-2023
line_day_current  = line_day_first
line_day_count    = (7, 14) # Note: changes with presence of teruglevering

# Day entry information:
with_teruglevering        = False
text_teruglevering        = 'Teruglevering'
offset_teruglevering_text = 4    # Note: used to probe for entry with 'Teruglevering'

# Offsets into a day's entry:
#      zonder teruglevering | met teruglevering
offset_datum           =  0
offset_levering        = (2 ,  2)
offset_teruglevering   =  5
offset_nettoverbruik   = (2 ,  9)
offset_vastekosten     = (6 , 13)
offset_variabelekosten = (3 , 10)
offset_totalekosten    = (0 ,  0)

# Vattenfall: Usage information for a single day to scrape from input file (at day_line(day)):
#
# A. Case without teruglevering:
# Ndx | Line contents       | Information
#  0  | 1 januari € 2,75    | [date] {Totale kosten}
#  1  | Variabele kosten    |
#  2  | 8,838 kWh           | {Levering}
#  3  | € 3,24              | {Variable Kosten}
#  4  | Vaste kosten        |
#  5  | 1 dg                |
#  6  | € -0,49             | {Vaste kosten}
#
# B. Case with teruglevering:
# Ndx | Line contents       | Information
#  0  | 8 mei € -2,15       | [date] {Totale kosten}
#  1  | Stroom              |
#  2  | 3,985 kWh           | {Levering}
#  3  | -                   |
#  4  | Teruglevering       |
#  5  | -13,862 kWh         | {Teruglevering}
#  6  | -                   |
#  7  | Netto verbruik      |
#  8  | Variabele kosten    |
#  9  | -9,877 kWh          | {Netto Verbruik}
# 10  | € -1,66             | {Variabele kosten}
# 11  | Vaste kosten        |
# 12  | 1 dg                |
# 13  | € -0,49             | {Vaste kosten}
#
# https://www.vattenfall.nl/service/mijn-vattenfall/#/vf/verbruik
#

# Excel sheet table to create from reading csv file (note: '€' is displayed due to column format):
#
# Datum	Levering [Wh]	Teruglevering [Wh]	Netto Verbruik [Wh]	Vaste Kosten	Variable Kosten	Totale kosten
# 01-01-23	8,838	0	8,838	 € -0.49 	 € 3.24 	 € 2.75
# ...
# 08-05-23	3,985	-13,862	-9,877	 € -0.49 	 € -1,66 	 € -2,15
#

# CSV format generated by script:
#
# Datum;Levering [Wh];Teruglevering [Wh];Netto Verbruik [Wh];Vaste Kosten;Variable Kosten;Totale kosten
# 01-01-23;8,838;0;8,838;-0.49;3.24;2.75
# ...
# 08-05-23;3,985;-13,862;-9,877;-0.49;-1,66;-2,15
#

# Methods to scrape:
#
# A. Expect format with teruglevering only, iterate through days in month, scrape info from fixed lines.
# B. Scan for day in month, determine kind of entry, scrape info accordingly.
#
# Method B. is used in this script.
#

def log(level, cmdargs, *args, **kwargs):
    """Print a log message, depending on its verbosity level"""
    if cmdargs.verbose >= level:
        print(*args, file=sys.stderr, **kwargs)

def eprint(*args, **kwargs):
    """Print an error"""
    print(*args, file=sys.stderr, **kwargs)

def wprint(*args, **kwargs):
    """Print a warning"""
    print(*args, file=sys.stderr, **kwargs)

def is_wildcard(path):
    """True if path contains a wildcard"""
    return '*' in path or '?' in path

def plural(text, count):
    """Return plural of text if count larger than one"""
    return text + ('s' if count > 1 else '')

def reset():
    """Reset state for processing the next file"""
    global line_day_current
    line_day_current  = line_day_first

def probe_teruglevering(lines, day):
    """Probe if currently selected day contains 'Terugleveren'."""
    # eprint('Probe teruglevering: {}'.format(lines[day_index(day) + offset_teruglevering_text]))
    return text_teruglevering in lines[day_index(day) + offset_teruglevering_text]

def determine_if_day_with_teruglevering(lines, day):
    """Determine if currently selected day contains 'Terugleveren', sets or resets flag accordingly."""
    global with_teruglevering
    with_teruglevering = probe_teruglevering(lines, day)
    return with_teruglevering

def advance_day():
    """Advance 'line with current day' to next day, taking presence or absence of 'Terugleveren' into account."""
    global line_day_current
    line_day_current = line_day_current + line_day_count[with_teruglevering]

def day_line(day):
    """Number of line with date for currently active day."""
    return line_day_current

def day_index(day):
    """Index into file content with date for currently active day."""
    return day_line(day) - 1

def days_in_month(year, month):
    """Number of days in given month and year."""
    return calendar.monthrange(year, month)[1]

def month_index():
    """Index into file content for line with month."""
    return line_month - 1

def scrape_year_month_days(lines):
    """Scrape year and month, return as numbers as (year, month, number_of_days_in_month)."""
    months = (
        'Skp',
        'Jan', 'Feb', 'Mrt',
        'Apr', 'Mei', 'Jun',
        'Jul', 'Aug', 'Sep',
        'Okt', 'Nov', 'Dec',
    )
    # eprint(lines[month_index()].split())
    month, year = lines[month_index()].split()
    yyyy = int(year)
    mm   = months.index(month)
    return (yyyy, mm, days_in_month(yyyy, mm))

def scrape_day_datum(lines, day):
    """Just return given day number."""
    return day

def to_date(year, month, day):
    """Return date as 'dd-mm-yyyy'."""
    return '{day}-{month}-{year}'.format(day=day, month=month, year=year)

def to_Wh(text):
    """Return number of Wh without separator."""
    return text.split()[0].replace(',', '')

def to_money(text):
    """Return amount with ',' replaced by '.', like '-1.23'."""
    # money provided as '-' (empty) or '€ 1,23' (variabele, vaste kosten) or '1 mei € 1,23' (levering)
    zero = '0' # '€ 1,23'
    if text == '-':
        return zero
    import re
    # split on and keep '€ ', and take last two elements:
    result = re.split('(€ )', text)[-2:]
    return (result[1]).replace(',', '.') if len(result) > 1 else zero
    # return (result[0] + result[1]).replace(',', '.') if len(result) > 1 else zero

def scrape_day_levering(lines, day):
    """Scrape 'Levering' for a day."""
    return to_Wh(lines[day_index(day) + offset_levering[with_teruglevering]])

def scrape_day_teruglevering(lines, day):
    """Scrape 'Teruglevering' for a day."""
    return to_Wh(lines[day_index(day) + offset_teruglevering]) if with_teruglevering else 0

def scrape_day_nettoverbruik(lines, day):
    """Scrape 'Netto Verbruik' for a day."""
    return to_Wh(lines[day_index(day) + offset_nettoverbruik[with_teruglevering]])

def scrape_day_vastekosten(lines, day):
    """Scrape 'Vaste Kosten' for a day."""
    return to_money(lines[day_index(day) + offset_vastekosten[with_teruglevering]])

def scrape_day_variabelekosten(lines, day):
    """Scrape 'Variabele Kosten' for a day."""
    return to_money(lines[day_index(day) + offset_variabelekosten[with_teruglevering]])

def scrape_day_totalelekosten(lines, day):
    """Scrape 'Totale Kosten' for a day."""
    return to_money(lines[day_index(day) + offset_totalekosten[with_teruglevering]])

def scrape_day(lines, day, month, year):
    """Scrape information for a single day."""
    determine_if_day_with_teruglevering(lines, day)
    # eprint('with_teruglevering:{}'.format(with_teruglevering))
    return (
        to_date(year, month, scrape_day_datum(lines, day)),
        scrape_day_levering(lines, day),
        scrape_day_teruglevering(lines, day),
        scrape_day_nettoverbruik(lines, day),
        scrape_day_vastekosten(lines, day),
        scrape_day_variabelekosten(lines, day),
        scrape_day_totalelekosten(lines, day),
    )

def scrape(src, args):
    """Scrape usage information from text file '{}'"""
    log(LOG_PROGRESS, args, scrape.__doc__.format(src))
    log(LOG_FILENAME, args, src)

    reset()

    import codecs
    with codecs.open(src, "r", "utf-8", errors='surrogateescape') as input:
        # read lines without newlines:
        lines = input.read().splitlines()

    year, month, days = scrape_year_month_days(lines)
    log(LOG_PROGRESS, args, 'year:{} month:{} days:{}'.format(year, month, days))

    result = []
    for day in range(1, days + 1):
        # eprint('Day: {}'.format(lines[day_index(day)]))
        result.append(scrape_day(lines, day, month, year))
        advance_day()

    return result

def report_header(args, output):
    """Report header"""
    log(LOG_PROGRESS, args, report_header.__doc__)
    print("Datum;Levering [Wh];Teruglevering [Wh];Netto Verbruik [Wh];Vaste Kosten;Variable Kosten;Totale kosten", file=output)

def report_entries(args, data, output):
    """Report entries"""
    log(LOG_PROGRESS, args, report_entries.__doc__)
    for entry in data:
        # eprint(entry)
        print('{date};{levering};{terug};{netto};{vast};{variabel};{totaal}'.format(
            date=entry[0], levering=entry[1], terug=entry[2], netto=entry[3],
            vast=entry[4], variabel=entry[5], totaal=entry[6]), file=output)

def report(args, data, output):
    """Report header and day entries, return 1 (processed one file)."""
    log(LOG_PROGRESS, args, report.__doc__.format(args.paths[0]))
    report_header(args, output)
    report_entries(args, data, output)
    return 1

def to_extension(path, ext):
    """Return path with extension replaced by ext."""
    return os.path.splitext(path)[0] + ext

def to_output_path(args, path):
    """Generate path for CSV output file based taking output folder from option '--csv-folder' into account"""
    # Two cases, depending on whether output folder is not specified on command line:
    # - is not specified: replace extension '.txt'. with '.csv;
    # - is specified: replace extension '.txt'. with '.csv and replace folder with provided one.
    if args.csv_folder:
        return os.path.join(args.csv_folder, to_extension(os.path.basename(path), '.csv'))
    else:
        return to_extension(path, '.csv')

def scrape_and_report_file(path, args, output):
    """Scrape text in specified file and create csv files of it; honours option -o, --output"""
    if os.path.isfile(path):
        return report(args, scrape(path, args), output)
    else:
        return 0

def scrape_and_report_folder(folder, args):
    """Scrape text files in folder' and create csv files of them."""
    count = 0
    for filename in os.listdir(folder):
        path = os.path.join(folder, filename)
        if os.path.isfile(path):
            log(LOG_PROGRESS, args, "Creating '{}'".format(to_output_path(args, path)))
            with open(to_output_path(args, path), 'w') as output:
                count = count + report(args, scrape(path, args), output)
    return count

def scrape_and_report_wildcard(wildcard, args):
    """Scrape text files in folder' and create csv files of them."""
    import glob
    count = 0
    for path in glob.glob(wildcard):
        if os.path.isfile(path):
            log(LOG_PROGRESS, args, "Creating '{}'".format(to_output_path(args, path)))
            with open(to_output_path(args, path), 'w') as output:
                count = count + report(args, scrape(path, args), output)
    return count

def scrape_and_report(args, output):
    """Scrape text file(s) '{}' and create csv file(s)."""
    log(LOG_PROGRESS, args, scrape_and_report.__doc__.format(args.paths[0]))
    count = 0
    try:
        for path in args.paths:
            if os.path.isfile(path):
                count = count + scrape_and_report_file(path, args, output)
            elif os.path.isdir(path):
                count = count + scrape_and_report_folder(path, args)
            elif not is_wildcard(path):
                wprint("Warning: file or folder '{}' not found".format(path))
            else:
                count = count + scrape_and_report_wildcard(path, args)
    except OSError as err:
        eprint('Error: {}'.format(err))
    if count > 0:
        log(LOG_PROCESSED, args, '{count} {files} processed'.format(count=count, files=plural('file', count)))
    else:
        wprint('Warning: not a single file processed')

def option_output(args):
    """..."""
    return args.output

def has_paths(args):
    """..."""
    return len(args.paths) > 0

def multiple_files(args):
    """..."""
    return len(args.paths) > 1 or not os.path.isfile(args.paths[0])

def error(text, status):
    """..."""
    eprint('Error: {}'.format(text))
    return status

def main():
    """Scrape given text file(s) with Vattenfall daily electricity usage and create a csv file of it."""

    parser = argparse.ArgumentParser(
        description="""Scrape given text file(s) with Vattenfall daily electricity usage and create file(s) in csv format.
Single file output is to stdout default and can be directed to a file using option '--output'.
When multiple files are specified, output is to a file of the same name with the extension replaced with '.csv'.
Multiple file output can be directed to a folder using option '--csv-folder'.
""",
        epilog="""""",
        formatter_class=argparse.ArgumentDefaultsHelpFormatter)
#        formatter_class=argparse.RawTextHelpFormatter)

    parser.add_argument(
        '-v', '--verbose',
        action='count',
        default=0,
        help='report file being processed (level 1), count (2), progress (3)')

    # parser.add_argument(
    #     '--input-folder',
    #     metavar='input',
    #     default=None,
    #     type=str,
    #     help='folder that contains source txt files')

    parser.add_argument(
        '--csv-folder',
        metavar='csv',
        default=None,
        type=str,
        help='folder to write csv files to')

    parser.add_argument(
        '--output',
        metavar='output',
        type=str,
        help='output file in csv format')

    parser.add_argument(
        'paths',
        metavar='paths',
        default='',
        type=str,
        nargs='+',
        help='file(s) with copy-pasted web page text (file, folder, wildcard)')

    args = parser.parse_args()

    # eprint(args)

    if option_output(args) and multiple_files(args):
        return error("can only use option '--output' with a single file", EXIT_FAILURE)

    if has_paths(args):
        if option_output(args):
            with open(args.output, 'w') as file:
                scrape_and_report(args, file)
        else:
            scrape_and_report(args, sys.stdout)
    else:
        parser.print_help()

if __name__ == '__main__':
    main()

# end of file
