-- ada/scoped.ads.
--
-- Copyright 2023 by Martin Moene
--
-- Distributed under the Boost Software License, Version 1.0.
-- (See accompanying file LICENSE.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

with Ada.Text_IO;
with Ada.Finalization;

package scoped is

type scoped_file is new Ada.Finalization.Limited_Controlled with record
    file: Ada.Text_IO.File_type;
end record;

procedure Finalize  (Self: in out scoped_file);
procedure Create    (Self: in out scoped_file; path: String);
procedure Open      (Self: in out scoped_file; path: String);

end scoped;
