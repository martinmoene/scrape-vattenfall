-- Note: Growing functionality.
--
-- Scrape given text file with Vattenfall per-day electricity usage for its entries and create a csv file of it.
--
-- Copyright 2023 by Martin Moene
--
-- Distributed under the Boost Software License, Version 1.0.
-- (See accompanying file LICENSE.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

--  Milestones:
--
--  - [x] Provide general command line handling, https://en.wikibooks.org/wiki/Ada_Programming/Libraries/Ada.Command_Line
--    - [x] differentiate between options and positional arguments.
--    - [x] split options into name, value.
--    - [x] collect options in a record.
--    - [x] collect positional arguments in a vector.
--    - [x] change usage of 'Positional_Arguments.Vector' to 'Positional_Arguments' via subtype
--  - [x] Message output
--    - [x] error.
--    - [x] warning.
--    - [x] log.
--  - [x] Read fixed text file, write to stdout, line by line.
--  - [x] Read fixed text file, store in array of lines, write to stdout, line by line.
--  - [x] Obtain filename(s) from command line.
--  - [x] String functions:
--    - [x] replace(text, substr, repl): solved with replace_chr(text, set, repl).
--    - [x] formatting w/o space for date '01-02-2023': to_string().
--  - [ ] Provide path and file operations:
--    - [x] isfile().
--    - [x] isdir().
--    - [x] get basename.
--    - [x] replace extension.
--    - [ ] join path elements.
--  - [x] Determine days in month, days_in_month(yyyy, mm); requires calendar of some sort: use is_leap_year().
--  - [x] Find equivalent of Python 'with' statement, or 'scoped_file' type.
--  - [x] Traverse folder
--  - [x] Expand wildcards: appears to be performed by command line module, Ada.Command_Line.
--  - [ ] With multiple files (or combinations), do not use stdoutput for the files, but write to properly named files instead.

with pair;
with scoped; use scoped;

with Ada.Command_Line;

with Ada.Directories;         use Ada.Directories;

with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Strings.Unbounded.Text_IO;

with Ada.Strings;             use Ada.Strings;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Characters.Latin_1;  use Ada.Characters.Latin_1;		-- for CR LF
--  with Ada.Characters.Handling; use Ada.Characters.Handling;
--  with Ada.Strings_Edit.UTF8.Handling;

with Ada.Containers;          use Ada.Containers;
with Ada.Containers.Vectors;

procedure scrape_electricity_per_day_vattenfall is

	package CLI   renames Ada.Command_Line;
	package IO    renames Ada.Text_IO;
	package INTIO renames Ada.Integer_Text_IO;
	package UBSIO renames Ada.Strings.Unbounded.Text_IO;

	LOG_FILENAME  : constant Natural := 1;
	LOG_PROCESSED : constant Natural := 2;
	LOG_PROGRESS  : constant Natural := 3;

  type Range_01    is range 0..1;
	type Number_Pair is array(Range_01) of Natural;

	-- Line information of text file copy-pasted from Vattenfal web page with a month's electricity usage per day:
	line_month      : constant Natural := 27;							-- Note: per 11-May-2023
	line_day_first  : constant Natural := 31;							-- Note: per 11-May-2023
	line_day_current:          Natural := line_day_first;
	line_day_count  : constant Number_Pair := (7, 14);		-- Note: changes with presence of teruglevering

	--  Day entry information:
	with_teruglevering : Range_01 := 0;
	text_teruglevering : constant String := "Teruglevering";
	offset_teruglevering_text: constant Natural := 4;			-- Note: used to probe for entry with 'Teruglevering'

	--  Offsets into a day's entry:
	--                            zonder teruglevering | met teruglevering
	offset_datum          : constant Natural     :=  0;
	offset_levering       : constant Number_Pair := (2 ,  2);
	offset_teruglevering  : constant Natural     :=  5;
	offset_nettoverbruik  : constant Number_Pair := (2 ,  9);
	offset_vastekosten    : constant Number_Pair := (6 , 13);
	offset_variabelekosten: constant Number_Pair := (3 , 10);
	offset_totalekosten   : constant Number_Pair := (0 ,  0);

	--
	-- Pair of unbounded strings to hold command line option/value pairs:
	--
	package String_Pair_Type is new Pair (T => Unbounded_String, U => Unbounded_String);
	subtype String_Pair is String_Pair_Type.Pair;

	function make_pair(a: in String; b: in String) return String_Pair is
	begin
		return (To_Unbounded_String(a), To_Unbounded_String(b));
	end make_pair;

	--
	-- Option flags and values:
	--
	--  type Options is tagged record
	type Options is record
		help           : Boolean := False;		-- -h, --help
		verbose        : Natural := 0;				-- -v, --verbose
		csv_folder     : Unbounded_String;		-- --csv_folder=csv
		output         : Unbounded_String;		-- --output=output
	end record;

	function has_csv_folder(opts: Options) return Boolean is
	begin
		return Length(opts.csv_folder) > 0;
	end has_csv_folder;

	function has_output(opts: in Options) return Boolean is
	begin
		return Length(opts.output) > 0;
	end has_output;

	--
	-- Positional arguments:
	--
	package String_Vector is new Ada.Containers.Vectors(Index_Type => Natural, Element_Type => Unbounded_String);

	subtype Lines is String_Vector.Vector;
	subtype Positional_Arguments is String_Vector.Vector;

	function has_paths(args: in Positional_Arguments) return Boolean is
	begin
		return args.Length > 0;
	end has_paths;

	function multiple_paths(args: in Positional_Arguments) return Boolean is
	begin
		return args.Length > 1;
	end multiple_paths;

	--
	-- Day_Result:
	--
	type Day_Result is record
		date            : Unbounded_String;
		levering        : Unbounded_String;
		teruglevering   : Unbounded_String;
		nettoverbruik   : Unbounded_String;
		vastekosten     : Unbounded_String;
		variabelekosten : Unbounded_String;
		totalekosten    : Unbounded_String;
	end record;

	package Day_Vector is new Ada.Containers.Vectors(Index_Type => Natural, Element_Type => Day_Result);
	subtype Days is Day_Vector.Vector;

	--
	-- Convenience string functions:
	--
	function split_at(text: in String; pos: in Integer) return String_Pair is
	begin
		return (To_Unbounded_String(text(text'First .. pos - 1)), To_Unbounded_String(text(pos + 1 .. text'Last)));
	end split_at;

	function split(text: in String; set: in String) return String_Pair is
	begin
		return split_at(text, Index(To_Unbounded_String(text), set, text'First));
	end split;

	function split_backward(text: in String; set: in String) return String_Pair is
	begin
		return split_at(text, Index(To_Unbounded_String(text), set, text'Last, Backward));
	end split_backward;

	function contains(text: in String; sub: in String) return Boolean is
	begin
		return Index(To_Unbounded_String(text), sub) > 0;
	end contains;

	function starts_with(text: in String; start: in String) return Boolean is
	begin
		return start in text(start'First..start'Length);
	end starts_with;

	function strip(text: in String; front: in String) return String is
	begin
		return (
			if starts_with(text, front)
				then To_String(Tail(To_Unbounded_String(text), text'Length - front'Length ))
				else text
		);
  end strip;

	function remove(text: in String; srch: in String) return String is
		pos: Natural;
	begin
		pos := Index(text, srch);
		return (
			if pos > 0
				then Delete(text, pos, pos + srch'Length - 1)
				else text
		);
	end remove;

	-- Replace characters from set srch by repl.
	function replace_chr(text: in String; srch: in String; repl: in Character) return String is
		pos: Natural;
		result: Unbounded_String;
	begin
		pos    := Index(text, srch);
		result := To_Unbounded_String(text);

		if pos > 0 then
			Replace_Element(result, pos, repl);
		end if;

		return To_String(result);
	end replace_chr;

	function replace_chr(text: in Unbounded_String; srch: in String; repl: in Character) return String is
	begin
		return replace_chr(To_String(text), srch, repl);
	end replace_chr;

	function plural(text: in String; count: in Natural) return String is
	begin
    return text & (if count = 1 then "" else "s");
	end plural;

	-- Format number without leading space.
	function to_string(value: Integer) return String is
		text: String(1..20);
	begin
		INTIO.Put(text, value);
		return Trim(text, Side => Both);
	end to_string;

	function to_integer(text: in Unbounded_String) return Natural is
	begin
		return Integer'Value(To_String(text));
	end to_integer;

function to_real(text: in String) return Float is
	begin
		return Float'Value(text);
	end to_real;

function to_real(text: in Unbounded_String) return Float is
	begin
		return to_real(To_String(text));
	end to_real;

	--
	-- Path operations:
	--
	function is_wildcard(text: in String) return Boolean is
	begin
		return contains(text, "*") or contains(text, "?");
	end is_wildcard;

	function is_wildcard(text: in Unbounded_String) return Boolean is
	begin
		return is_wildcard(To_String(text));
	end is_wildcard;

	function os_path_isfile(arg: in Unbounded_String) return Boolean is
	begin
		return Exists(To_String(arg)) and then Kind (To_String(arg)) = Ordinary_File;
	end os_path_isfile;

	function os_path_isdir(arg: in Unbounded_String) return Boolean is
	begin
		return Exists(To_String(arg)) and then Kind (To_String(arg)) = Directory;
	end os_path_isdir;

	-- take last folder component (handle '/' and '\'), and remove extension (path/to/filename.ext => filename):
	function os_path_basename(text: in Unbounded_String) return Unbounded_String is
	begin
		return split(To_String(
						split_backward(To_String(
						split_backward(To_String(text), "/").rhs), "\\").rhs), ".").lhs;
	end os_path_basename;

	-- Python:
	-- os.path.join(path, *paths)
	--
	-- Join one or more path components intelligently.
	-- The return value is the concatenation of path and any members of *paths with exactly one directory separator (os.sep)
	-- following each non-empty part except the last, meaning that the result will only end in a separator if the last part is empty.
	-- If a component is an absolute path, all previous components are thrown away and joining continues from the absolute path component.
	--
	-- On Windows, the drive letter is not reset when an absolute path component (e.g., r'\foo') is encountered.
	-- If a component contains a drive letter, all previous components are thrown away and the drive letter is reset.
	-- Note that since there is a current directory for each drive, os.path.join("c:", "foo") represents a path relative to
	-- the current directory on drive C: (c:foo), not c:\foo.
	-- Changed in version 3.6: Accepts a path-like object for path and paths.

	function os_path_join(head: in Unbounded_String; tail: in Unbounded_String) return Unbounded_String is
	begin
		return head & To_Unbounded_String("/") & tail; -- TODO file/path: join
	end os_path_join;

	function os_listdir(folder: Unbounded_String) return Lines is
		result : Lines;
		Search : Search_Type;
		Dir_Ent: Directory_Entry_Type;
	begin
		Start_Search(Search, To_String(folder), "");

		while More_Entries(Search) loop
			Get_Next_Entry(Search, Dir_Ent);
			result.Append(To_Unbounded_String(Simple_Name(Dir_Ent)));
		end loop;
		End_Search(Search);

		return result;
	end os_listdir;

	--
	-- Main program:
	--
	procedure error(text: in String) is
	begin
		raise PROGRAM_ERROR with "Error: " & text;
	end error;

	procedure warning(text: in String) is
	begin
		IO.Put_Line(IO.Standard_Error, text);
	end warning;

	procedure log(level: in Natural; opts: in Options; text: in String) is
	begin
		if level <= opts.verbose then
			IO.Put_Line(IO.Standard_Error, text);
		end if;
	end log;

	procedure print_command is
	begin
		IO.Put_Line("Command Name: " & CLI.Command_Name);
	end print_command;

	procedure print_argc is
	begin
		IO.Put_Line("Argument Count:" & CLI.Argument_Count'Img);
	end print_argc;

	procedure print_list_num(i: in Natural) is
	begin
			IO.Put(i'Img & ": ");
	end print_list_num;

	procedure print_option(option: in String; name: in String; value: in Unbounded_String) is
	begin
				IO.Put("option: " & option & ": "); UBSIO.Put_Line(name & "/" & value);
	end print_option;

	procedure print_positional( arg: in String) is
	begin
		IO.Put_Line("positional: " & arg);
	end print_positional;

	procedure print_options( opts: in Options) is
	begin
		IO.Put_Line("Options:");
		IO.Put_Line("- help      :" & opts.help'Img);
		IO.Put_Line("- verbose   :" & opts.verbose'Img);
		IO.Put_Line("- csv-folder:" & has_csv_folder(opts)'Img & " / " & To_String(opts.csv_folder));
		IO.Put_Line("- output    :" & has_output(opts)'Img     & " / " & To_String(opts.output));
	end print_options;

	procedure print_arguments(args: in Positional_Arguments) is
	begin
		IO.Put_Line("Positional arguments:");
		for arg of args loop
			IO.Put_Line("- " & To_String(arg));
		end loop;
	end print_arguments;

	-- procedure print_filename(arg: in Unbounded_String) is
	-- begin
	-- 	IO.Put_Line(To_String(arg) & ":");
	-- end print_filename;

	function make_option_pair(arg: in String) return String_Pair is
	begin
		return (if starts_with(arg, "-") then (if contains(arg, "=") then split(arg, "=") else make_pair(arg, "")) else make_pair("", ""));
	end make_option_pair;

	procedure reset is
	begin
		line_day_current := line_day_first;
	end reset;

	function day_line(day: in Positive) return Natural is
	begin
		return line_day_current;
	end day_line;

	function day_index(day: in Positive) return Natural is
	begin
    return day_line(day) - 1;
	end day_index;

	function month_index return Positive is
	begin
    return line_month - 1;
	end month_index;

	function probe_teruglevering(line: in Lines; day: in Positive) return Boolean is
	begin
		-- eprint('Probe teruglevering: {}'.format(lines[day_index(day) + offset_teruglevering_text]))
		return contains(To_String(line(day_index(day) + offset_teruglevering_text)), text_teruglevering);
	end probe_teruglevering;

	procedure determine_if_day_with_teruglevering(line: in Lines; day: in Positive) is
	begin
		with_teruglevering := (if probe_teruglevering(line, day) then 1 else 0);
	end determine_if_day_with_teruglevering;

	procedure advance_day is
	begin
		line_day_current := line_day_current + line_day_count(with_teruglevering);
	end;

	-- Take number at left, replace ',' with '.', times 1000, changing from kWh to Wh ("6,505 kWh" => "6505"):
	function to_Wh(text: Unbounded_String) return Unbounded_String is
	begin
		-- omit leading space via to_string():
		return To_Unbounded_String(
			to_string(Integer(Float'Rounding(
					1000.0 * to_real(replace_chr(split(To_String(text), " ").lhs, ",", '.'))
			)))
		);
	end to_Wh;

	-- Take amount at right, replace ',' with '.' ("[1 mei ]€ 1,23" => 1.23):
	function to_money(text: Unbounded_String) return Unbounded_String is
		dash  : constant Unbounded_String := To_Unbounded_String("-");
		zero  : constant Unbounded_String := To_Unbounded_String("0");
		amount: Unbounded_String;
	begin
		if text = dash then
				return zero;
		end if;

		amount := split_backward(To_String(text), " ").rhs;

		return (
			if Length(amount) > 1
				--  then To_Unbounded_String(replace(To_String(amount), ",", "."))
				then To_Unbounded_String(replace_chr(To_String(amount), ",", '.'))
				else zero
		);
	end to_money;

	function scrape_day_levering(line: in Lines; day: in Positive) return Unbounded_String is
	begin
		return to_Wh(line(day_index(day) + offset_levering(with_teruglevering)));
	end scrape_day_levering;

	function scrape_day_teruglevering(line: in Lines; day: in Positive) return Unbounded_String is
	begin
		return (if with_teruglevering = 1 then to_Wh(line(day_index(day) + offset_teruglevering)) else To_Unbounded_String("0"));
	end scrape_day_teruglevering;

	function scrape_day_nettoverbruik(line: in Lines; day: in Positive) return Unbounded_String is
	begin
		return to_Wh(line(day_index(day) + offset_nettoverbruik(with_teruglevering)));
	end scrape_day_nettoverbruik;

	function scrape_day_vastekosten(line: in Lines; day: in Positive) return Unbounded_String is
	begin
		return to_money(line(day_index(day) + offset_vastekosten(with_teruglevering)));
	end scrape_day_vastekosten;

	function scrape_day_variabelekosten(line: in Lines; day: in Positive) return Unbounded_String is
	begin
		return to_money(line(day_index(day) + offset_variabelekosten(with_teruglevering)));
	end scrape_day_variabelekosten;

	function scrape_day_totalelekosten(line: in Lines; day: in Positive) return Unbounded_String is
	begin
		return to_money(line(day_index(day) + offset_totalekosten(with_teruglevering)));
	end scrape_day_totalelekosten;

	function to_date(year: in Positive; month: in Positive; day: in Positive) return Unbounded_String is
	begin
		return To_Unbounded_String(to_string(day) & "-" & to_string(month) & "-" & to_string(year));
	end to_date;

	function is_leap_year(year: Positive) return Boolean is
	begin
		-- See https://rosettacode.org/wiki/Leap_year#Ada.
		return (year rem 4 = 0) and then ((year rem 16 = 0) or else (year rem 100 /= 0));
	end;

	function days_in_month(year: in Positive; month: in Positive) return Natural is
	begin
		case month is
				when 1 | 3 | 5 | 7 | 8 | 10 | 12 => return 31;
				when 4 | 6 | 9 | 11              => return 30;
				when 2 => return (if is_leap_year(year) then 29 else 28);
				when others => return 0;
		end case;
	end days_in_month;

	type YearMonth is record
		year : Positive;
		month: Positive;
		days : Positive;
	end record;

	function to_month_num(month: in Unbounded_String) return Natural is
		type MonthList is array(1..12) of String(1..3);
		name: constant MonthList := (
		    "Jan", "Feb", "Mrt",
		    "Apr", "Mei", "Jun",
		    "Jul", "Aug", "Sep",
		    "Okt", "Nov", "Dec"
		);
	begin
		for i in name'Range loop
			if month = name(i) then
				return i;
			end if;
		end loop;
		return 0;
	end to_month_num;

	function scrape_year_month_days(data: in Lines) return YearMonth is
		month: String_Pair := make_pair("", "");
		yyyy : Positive;
		mm   : Positive;
	begin
		month := split(To_String(data(month_index)), " ");
		mm    := to_month_num(month.lhs);
		yyyy  := to_integer(month.rhs);

		return (yyyy, mm, days_in_month(yyyy, mm));
	end scrape_year_month_days;

	function scrape_day(line: in Lines; day: in Positive; month: in Positive; year: in Positive) return Day_Result is
	begin
    determine_if_day_with_teruglevering(line, day);
    -- eprint('with_teruglevering:{}'.format(with_teruglevering))
    return (
        to_date(year, month, day),	--  to_date(year, month, scrape_day_datum(line, day)),
        scrape_day_levering(line, day),
        scrape_day_teruglevering(line, day),
        scrape_day_nettoverbruik(line, day),
        scrape_day_vastekosten(line, day),
        scrape_day_variabelekosten(line, day),
        scrape_day_totalelekosten(line, day)
    );
	end scrape_day;

	-- return a named file's content as Lines.
	function read_file(src: in Unbounded_String) return Lines is
		input : scoped_file;
		line  : Unbounded_String;
		result: Lines;
	begin
		input.open(To_String(src));
		loop
			exit when IO.End_Of_File(input.file);
			UBSIO.Get_Line(input.file, line);
			result.Append(line);
		end loop;
		return result;
	end read_file;

	-- return a file's scraped content as Days.
	function scrape_content(data: in Lines; opts: in Options) return Days is
		month : YearMonth;
		result: Days;
	begin
		reset;
		month := scrape_year_month_days(data);
		log(LOG_PROGRESS, opts, "year:" & to_string(month.year) & " month:" & to_string(month.month) & " days:" & to_string(month.days));

		for day in 1..month.days loop
			-- eprint('Day: {}'.format(lines[day_index(day)]))
			result.Append(scrape_day(data, day, month.month, month.year));
			advance_day;
		end loop;
		return result;
	end scrape_content;

	-- return a named file's scraped content as Days.
	function scrape(src: in Unbounded_String; opts: in Options) return Days is
	begin
		log(LOG_FILENAME, opts, To_String(src) & ":");

		return scrape_content(read_file(src), opts);
	end;

	function to_extension(path: in Unbounded_String; ext: in String) return Unbounded_String is
	begin
		return split_backward(To_String(path), ".").lhs & To_Unbounded_String(ext);
	end to_extension;

	function to_output_path(opts: in Options; path: in Unbounded_String) return  Unbounded_String is
	begin
		-- Two cases, depending on whether output folder is specified on command line:
		-- - is not specified: replace extension '.txt'. with '.csv;
		-- - is specified: replace extension '.txt'. with '.csv and replace folder with provided one.
		if has_csv_folder(opts) then
				return os_path_join(opts.csv_folder, os_path_basename(path) & ".csv");
		else
				return to_extension(path, ".csv");
		end if;
	end to_output_path;

	procedure report_header(opts: in Options; output: in IO.File_Type) is
	begin
    log(LOG_PROGRESS, opts, "report_header.");
    IO.Put_Line(output, "Datum;Levering [Wh];Teruglevering [Wh];Netto Verbruik [Wh];Vaste Kosten;Variable Kosten;Totale kosten");
	end report_header;

	procedure report_entries(opts: in Options; data: in Days; output: in IO.File_Type) is
		sep: constant Unbounded_String := To_Unbounded_String(";");
	begin
		log(LOG_PROGRESS, opts, "report_entries.");

		for elem of data loop
			UBSIO.Put_Line(output,
				elem.date & sep &
				elem.levering    & sep & elem.teruglevering   & sep & elem.nettoverbruik & sep &
				elem.vastekosten & sep & elem.variabelekosten & sep & elem.totalekosten
			);
		end loop;
	end report_entries;

	function report(opts: in Options; data: in Days; output: in IO.File_Type) return Natural is
	begin
		log(LOG_PROGRESS, opts, "report.");

		report_header(opts, output);
		report_entries(opts, data, output);
    return 1;
	end report;

	function scrape_and_report_file(opts: in Options; path: in Unbounded_String; output: in IO.File_Type ) return Natural is
	begin
    if os_path_isfile(path) then
        return report(opts, scrape(path, opts), output);
    else
        return 0;
		end if;
	end scrape_and_report_file;

	function scrape_and_report_folder(opts: in Options; folder: in Unbounded_String) return Natural is
		count: Natural := 0;
		path : Unbounded_String;
	begin
		if not has_csv_folder(opts) then
			error("can only process folder when destination is specified via option '--csv-folder'.");
		end if;

		if folder = opts.csv_folder then
			error("folder to process must differ from folder specified with option '--csv-folder', both are: '" & To_String(folder) & "'.");
		end if;

		count := 0;
		for filename of os_listdir(folder) loop
				path := os_path_join(folder, filename);
				if os_path_isfile(path) then
					log(LOG_PROGRESS, opts, "Creating '" & To_String(to_output_path(opts, path)) & "'");
					declare
						output: scoped_file;
					begin
						output.create(To_String(to_output_path(opts, path)));
						count := count + report(opts, scrape(path, opts), output.file);
					end;
				end if;
		end loop;
		return count;
	end scrape_and_report_folder;

	-- Wildcards are handled by the command line module, it appears: no implementation required.
	function scrape_and_report_wildcard(opts: in Options; path: in Unbounded_String) return Natural is
	begin
		return 0;
	end scrape_and_report_wildcard;

	-- TODO file/path: with multiple files (via wild cards or completely specified), write to file instead of stdout.
	procedure scrape_and_report(opts: in Options; args: in Positional_Arguments; output: in IO.File_Type ) is
		count: Natural := 0;
	begin
		for path of args loop
			if os_path_isfile(path) then
					count := count + scrape_and_report_file(opts, path, output);
			elsif os_path_isdir(path) then
					count := count + scrape_and_report_folder(opts, path);
			elsif not is_wildcard(path) then
					warning("Warning: file or folder '" & To_String(path) & "' not found.");
			else
					count := count + scrape_and_report_wildcard(opts, path);
			end if;
		end loop;

		log(LOG_PROCESSED, opts, to_string(count) & " " & plural("file", count) & " processed.");
		if count = 0 then
			warning("Warning: not a single file processed.");
		end if;
	end scrape_and_report;

	procedure print_help is
		usage : constant String := CR & LF &
			"Usage: scrape_electricity_per_day_vattenfall [-h] [-v] [--csv-folder=csv] [--output=output] paths [paths ...]" & CR & LF &
			CR & LF &
			"Scrape given text file(s) with Vattenfall daily electricity usage and create file(s) in csv format. Single file output is to stdout default and can be directed to a file using option '--output'. When multiple files are specified, output is to a file of the same name with the extension replaced with '.csv'. Multiple file output can be directed to a folder using option '--csv-folder'." & CR & LF &
			CR & LF &
			"positional arguments:" & CR & LF &
			"  paths             file(s) with copy-pasted web page text (file, folder, wildcard)" & CR & LF &
			CR & LF &
			"optional arguments:" & CR & LF &
			"  -h, --help        show this help message and exit" & CR & LF &
			"  -v, --verbose     report file being processed (level 1), count (2), progress (3) (default: 0)" & CR & LF &
			"  --csv-folder=csv  folder to write csv files to (default: None)" & CR & LF &
			"  --output=output   output file in csv format (default: None)";
	begin
		IO.Put_Line(usage);
	end print_help;

--
-- Main block:
--
	opts : Options;
	args : Positional_Arguments;

begin
	-- print_command;
	-- print_argc;

	-- handle options and positional arguments:
	for i in 1 .. CLI.Argument_Count loop
		declare
			arg      : constant String           := CLI.Argument(i);
			opt      : constant String_Pair      := make_option_pair(arg);
			opt_name : constant String           := To_String(opt.lhs);
			opt_value: constant Unbounded_String := opt.rhs;
		begin
			-- print_list_num(i);
			if starts_with(arg, "-") then
				-- print_option(arg, opt_name, opt_value);
				if    "--csv-folder" = opt_name then opts.csv_folder := opt_value;
				elsif "--output"     = opt_name then opts.output     := opt_value;
				elsif "--help"       = opt_name or "-h" = opt_name then opts.help    := True;
				elsif "--verbose"    = opt_name or "-v" = opt_name then opts.verbose := opts.verbose + 1;
				elsif                             "-vv" = opt_name then opts.verbose := opts.verbose + 2;
				elsif                            "-vvv" = opt_name then opts.verbose := opts.verbose + 3;
				else  error("unrecognised option '" & arg & "'.");
				end if;
			else
				-- print_positional(arg);
				args.Append(To_Unbounded_String(arg));
			end if;
		end;
	end loop;

	if opts.verbose >= LOG_PROGRESS then
		print_options(opts);
		print_arguments(args);
	end if;

	if has_output(opts) and multiple_paths(args) then
		error("can only use option '--output' with a single file.");
	end if;

  if opts.help or not has_paths(args) then
		print_help;
	else
		declare
			output: scoped_file;
		begin
			if has_output(opts) then
				output.create(To_String(opts.output));
				scrape_and_report(opts, args, output.file);
			else
				scrape_and_report(opts, args, IO.Standard_Output);
			end if;
		end;
	end if;
end scrape_electricity_per_day_vattenfall;
