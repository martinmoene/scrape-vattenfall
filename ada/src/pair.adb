-- ada/pair.adb.
--
-- Copyright 2023 by Martin Moene
--
-- Distributed under the Boost Software License, Version 1.0.
-- (See accompanying file LICENSE.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

-- Added for demonstration; can simply do: `p : Num_Pair := (1, 2)`;

package body Pair is
	function make_pair(lhs: in T; rhs: in U) return Pair is
	begin
		return (lhs, rhs);
	end make_pair;
end Pair;
