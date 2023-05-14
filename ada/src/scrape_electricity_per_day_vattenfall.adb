-- Note: 'Hello, World!' placeholder for now to setup Ada environment using GNAT toolset.
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
--    - [ ] collect options in a record
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
	package IO renames Ada.Text_IO;
	package UBIO renames Ada.Strings.Unbounded.Text_IO;
	package CLI renames Ada.Command_Line;

	type String_Pair is record
		a: Unbounded_String;
		b: Unbounded_String;
	end record;

	type Options is record
		help           : Boolean := False;		-- -h, --help
		verbose        : Integer := 0;				-- -v, --verbose
		csv_folder_flag: Boolean := False;		-- --csv_folder flag
		csv_folder     : Unbounded_String;		-- --csv_folder=csv
		output_flag    : Boolean := False;		-- --output flag
		output         : Unbounded_String;		-- --output=output
	end record;

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

	opts : Options;
	empty_pair : constant String_Pair := (To_Unbounded_String(""), To_Unbounded_String(""));

begin
	IO.Put_Line ("Command Name: " & CLI.Command_Name);
	IO.Put_Line ("Argument Count:" & CLI.Argument_Count'Img);

	--  handle options and positional arguments:
	for i in 1 .. CLI.Argument_Count loop
		declare
			arg : constant String      := CLI.Argument(i);
			opt : constant String_Pair := (if starts_with(arg, "--") then split(strip(arg, "--"), "=") else empty_pair);
		begin
			IO.Put(i'Img & ": ");
			if starts_with(arg, "--") then
				IO.Put("option: " & arg & ": "); UBIO.Put_Line(opt.a & "/" & opt.b);
				--  '--option=value':
				if contains(arg, "=") then
					if "csv-folder" in To_String(opt.a) then
						opts.csv_folder_flag := True;
						opts.csv_folder      := opt.b;
					end if;
					if "output" in To_String(opt.a) then
						opts.output_flag := True;
						opts.output      := opt.b;
					end if;
				else	-- '--option' without value:
					if    arg = "--help"    then opts.help := True;
					elsif arg = "--verbose" then opts.verbose := opts.verbose + 1;
					else  IO.Put_Line("Unrecognised option '" & arg & "'.");
					end if;
				end if;
			else
				IO.Put_Line("positional: " & arg & ": ");
			end if;
		end;
	end loop;
	IO.Put_Line("Options:");
	IO.Put_Line("help      :" & opts.help'Img);
	IO.Put_Line("verbose   :" & opts.verbose'Img);
	IO.Put_Line("csv-folder:" & opts.csv_folder_flag'Img & " / " & To_String(opts.csv_folder));
	IO.Put_Line("output    :" & opts.output_flag'Img     & " / " & To_String(opts.output));
end scrape_electricity_per_day_vattenfall;
