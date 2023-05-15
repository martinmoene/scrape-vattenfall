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
--  - [ ] Provide general command line handling, https://en.wikibooks.org/wiki/Ada_Programming/Libraries/Ada.Command_Line
--    - [x] differentiate between options and positional arguments
--    - [x] split options into name, value
--    - [x] collect options in a record
--    - [ ] collect positional arguments in an array
--  - [ ] Read fixed text file, write to stdout, line by line.
--  - [ ] Read fixed text file, store in array of lines, write to stdout, line by line.
--  - [ ] Obtain filename from command line
--  - [ ] Provide path operations
--    - [ ] get basename
--    - [ ] replace extension
--    - [ ] join path elements

with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO;

procedure scrape_electricity_per_day_vattenfall is
	package IO   renames Ada.Text_IO;
	package UBIO renames Ada.Strings.Unbounded.Text_IO;
	package CLI  renames Ada.Command_Line;

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

	--
	-- Main program:
	--
	procedure error(text: in String) is
	begin
		raise PROGRAM_ERROR with "Error: " & text;
	end error;

	procedure warning(text: in String) is
	begin
		IO.Put_Line("Warning: " & text);
	end warning;

	procedure print_command is
	begin
		IO.Put_Line ("Command Name: " & CLI.Command_Name);
	end print_command;

	procedure print_argc is
	begin
		IO.Put_Line ("Argument Count:" & CLI.Argument_Count'Img);
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

	opts : Options;

begin
	print_command;
	print_argc;

	-- handle options and positional arguments:
	for i in 1 .. CLI.Argument_Count loop
		declare
			arg      : constant String           := CLI.Argument(i);
			opt      : constant String_Pair      := (if starts_with(arg, "-") then (if contains(arg, "=") then split(arg, "=") else make_pair(arg, "")) else make_pair("", ""));
			opt_name : constant String           := To_String(opt.a);
			opt_value: constant Unbounded_String := opt.b;
		begin
			print_list_num(i);
			if starts_with(arg, "-") then
				print_option(arg, opt_name, opt_value);
				if    "--csv-folder" = opt_name then opts.csv_folder := opt_value;
				elsif "--output"     = opt_name then opts.output     := opt_value;
				elsif "--help"       = opt_name or "-h" = opt_name then opts.help    := True;
				elsif "--verbose"    = opt_name or "-v" = opt_name then opts.verbose := opts.verbose + 1;
				else  error("unrecognised option '" & arg & "'.");
				end if;
			else
				print_positional(arg);
			end if;
		end;
	end loop;
	print_options(opts);
end scrape_electricity_per_day_vattenfall;
