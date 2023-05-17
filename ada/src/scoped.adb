-- ada/scoped.adb.
--
-- Copyright 2023 by Martin Moene
--
-- Distributed under the Boost Software License, Version 1.0.
-- (See accompanying file LICENSE.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

with Ada.Text_IO; use Ada.Text_IO;

package body scoped is

package IO renames Ada.Text_IO;

procedure Finalize(Self: in out scoped_file) is
begin
	--  Put_Line ("scoped_file: Finalize: close file");
	if Is_Open(self.file) then
			Close(self.file);
	end if;
end;

procedure Create(Self: in out scoped_file; path: String) is
begin
	--  Put_Line ("scoped_file: Create file '" & path & "'");
	Create(self.file, IO.Out_File, path);
end;

procedure Open(Self: in out scoped_file; path: String) is
begin
	--  Put_Line ("scoped_file: Open file '" & path & "'");
  Open(self.file, IO.In_File, path);
end;

begin
	null;
    --  Put_Line ("*** Start");
    --  declare
    --      f: scoped_file;
    --  begin
		--  		f.create("effe.txt");	-- create(f, "effe.txt");
    --      Put_line(f.file, "write some text");
    --  end;
    --  Put_Line ("*** End");
end scoped;
