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

	function split_at(text: in String; pos: in Integer) return String_Pair is
	begin
		return (To_Unbounded_String(text(text'First .. pos - 1)), To_Unbounded_String(text(pos + 1 .. text'Last)));
	end split_at;

	function split(text: in String; set: in String) return String_Pair is
	begin
		return split_at(text, Index(To_Unbounded_String(text), set, 1));
	end split;

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

begin
	IO.Put_Line ("Command Name: " & CLI.Command_Name);
	IO.Put_Line ("Argument Count:" & CLI.Argument_Count'Img);
	for i in 1 .. CLI.Argument_Count loop
		declare
			arg : constant String := CLI.Argument(i);
			opt : constant String := strip(arg, "--");
		begin
			IO.Put(i'Img & ": ");
			if starts_with(arg, "--") then
				IO.Put("option: " & opt & ": "); UBIO.Put_Line(split(opt,"=").a & "/" & split(opt,"=").b);
			else
				IO.Put_Line("positional: " & arg & ": ");
			end if;
		end;
	end loop;
end scrape_electricity_per_day_vattenfall;
