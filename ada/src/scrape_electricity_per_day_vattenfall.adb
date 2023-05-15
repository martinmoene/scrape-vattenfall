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
--    - [x] somehow change usage of 'Positional_Arguments.Vector' to 'Positional_Arguments'
--  - [x] Message output
--    - [x] error.
--    - [x] warning.
--    - [x] log.
--  - [ ] Read fixed text file, write to stdout, line by line.
--  - [ ] Read fixed text file, store in array of lines, write to stdout, line by line.
--  - [ ] Obtain filename from command line.
--  - [ ] Provide path and file operations:
--    - [ ] get basename.
--    - [ ] replace extension.
--    - [ ] join path elements.
--   	- [ ] scoped_file type (equivalent of Python with).

with Ada.Text_IO;
with Ada.Command_Line;

with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO;

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;

with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;

-- with Ada.Directories;

procedure scrape_electricity_per_day_vattenfall is
	package IO   renames Ada.Text_IO;
	package UBIO renames Ada.Strings.Unbounded.Text_IO;
	package CLI  renames Ada.Command_Line;

	LOG_FILENAME  : constant Natural := 1;
	LOG_PROCESSED : constant Natural := 2;
	LOG_PROGRESS  : constant Natural := 3;

	--
	-- Pair of unbounded strings to hold command line option/value pairs:
	--
	type String_Pair is record
		a: Unbounded_String;
		b: Unbounded_String;
	end record;

	function make_pair(a: in String; b: in String) return String_Pair is
	begin
		return (To_Unbounded_String(a), To_Unbounded_String(b));
	end make_pair;

	--
	-- Option flags and values:
	--
	type Options is record
		help           : Boolean := False;		-- -h, --help
		verbose        : Natural := 0;				-- -v, --verbose
		csv_folder     : Unbounded_String;		-- --csv_folder=csv
		output         : Unbounded_String;		-- --output=output
	end record;

	function has_csv_folder(opts: in Options) return Boolean is
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
	package Positional_Arguments is new Ada.Containers.Vectors(Index_Type => Natural, Element_Type => Unbounded_String);

	function has_paths(args: in Positional_Arguments.Vector) return Boolean is
	begin
		return args.Length > 0;
	end has_paths;

	function multiple_paths(args: in Positional_Arguments.Vector) return Boolean is
	begin
		return args.Length > 1;
	end multiple_paths;

	--
	-- Convenience string functions:
	--
	function split_at(text: in String; pos: in Integer) return String_Pair is
	begin
		return (To_Unbounded_String(text(text'First .. pos - 1)), To_Unbounded_String(text(pos + 1 .. text'Last)));
	end split_at;

	function split(text: in String; set: in String) return String_Pair is
	begin
		return split_at(text, Index(To_Unbounded_String(text), set, 1));
	end split;

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

	function plural(text: in String; count: in Natural) return String is
	begin
    return text & (if count > 1 then "s" else "");
	end plural;

	function is_wildcard(text: in String) return Boolean is
	begin
		return contains(text, "*") or contains(text, "?");
	end is_wildcard;

	function is_wildcard(text: in Unbounded_String) return Boolean is
	begin
		return is_wildcard(To_String(text));
	end is_wildcard;

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

	procedure print_option( option: in String; name: in String; value: in Unbounded_String) is
	begin
				IO.Put("option: " & option & ": "); UBIO.Put_Line(name & "/" & value);
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

	procedure print_arguments(args: in Positional_Arguments.Vector) is
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

	function os_path_isfile(arg: in Unbounded_String) return Boolean is
	begin
		return False; -- Ada.Files.Exists(To_String(arg));
	end os_path_isfile;

	function os_path_isdir(arg: in Unbounded_String) return Boolean is
	begin
		return False; -- Ada.Directories.Exists(To_String(arg));
	end os_path_isdir;

	function scrape(src: in Unbounded_String; opts: in Options) return Natural is -- return Type: lines
	begin
		log(LOG_FILENAME, opts, To_String(src) & ":");
		return 0;
	end;

	function scrape_and_report_file(opts: in Options; path: in Unbounded_String; output: in IO.File_Type ) return Natural is
	begin
		return 0;
	end scrape_and_report_file;

	function scrape_and_report_folder(opts: in Options; path: in Unbounded_String) return Natural is
	begin
		return 0;
	end scrape_and_report_folder;

	function scrape_and_report_wildcard(opts: in Options; path: in Unbounded_String) return Natural is
	begin
		return 0;
	end scrape_and_report_wildcard;

	procedure scrape_and_report(opts: in Options; args: in Positional_Arguments.Vector; output: in IO.File_Type ) is
		count: Natural := 0;
	begin
		for path of args loop
			log(LOG_FILENAME, opts, To_String(path) & ":");

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

		if count > 0 then
			log(LOG_PROCESSED, opts, "{count} {files} processed."); -- .format(count=count, files=plural('file', count)))
		else
			warning("Warning: not a single file processed.");
		end if;
	end scrape_and_report;

	procedure print_help is
		usage : constant String := CR & LF &
			"Usage: scrape_electricity_per_day_vattenfall [-h] [-v] [--csv-folder csv] [--output output] paths [paths ...]" & CR & LF &
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
	args : Positional_Arguments.Vector;

begin
	-- print_command;
	-- print_argc;

	-- handle options and positional arguments:
	for i in 1 .. CLI.Argument_Count loop
		declare
			arg      : constant String           := CLI.Argument(i);
			opt      : constant String_Pair      := make_option_pair(arg);
			opt_name : constant String           := To_String(opt.a);
			opt_value: constant Unbounded_String := opt.b;
		begin
			-- print_list_num(i);
			if starts_with(arg, "-") then
				-- print_option(arg, opt_name, opt_value);
				if    "--csv-folder" = opt_name then opts.csv_folder := opt_value;
				elsif "--output"     = opt_name then opts.output     := opt_value;
				elsif "--help"       = opt_name or "-h" = opt_name then opts.help    := True;
				elsif "--verbose"    = opt_name or "-v" = opt_name then opts.verbose := opts.verbose + 1;
				else  error("unrecognised option '" & arg & "'.");
				end if;
			else
				-- print_positional(arg);
				args.Append(To_Unbounded_String(arg));
			end if;
		end;
	end loop;

	-- print_options(opts);
	-- print_arguments(args);

	if has_output(opts) and multiple_paths(args) then
		error("can only use option '--output' with a single file.");
	end if;

  if opts.help or not has_paths(args) then
		print_help;
	else
		declare
			output: IO.File_Type;
		begin
			if has_output(opts) then
				IO.Create(output, IO.Out_File, To_String(opts.output));
				scrape_and_report(opts, args, output);
				IO.Close(output);
			else
				scrape_and_report(opts, args, IO.Standard_Output);
			end if;
		end;
	end if;
end scrape_electricity_per_day_vattenfall;
