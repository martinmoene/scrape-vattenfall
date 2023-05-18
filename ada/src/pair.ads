-- ada/pair.ads.
--
-- Copyright 2023 by Martin Moene
--
-- Distributed under the Boost Software License, Version 1.0.
-- (See accompanying file LICENSE.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

-- Example usage:
--   package Num_Pair_Type is new Pair (T => Natural, U => Natural);
--   subtype Num_Pair is Num_Pair_Type.Pair;
--   value : Num_Pair := (1, 2); -- or: Num_Pair_Type.make_pair(1, 2);

generic
   type T is private;
   type U is private;
package Pair is
   type Pair is tagged record
      lhs: T;
      rhs: U;
   end record;

	-- Added for demonstration; can simply do: `p : Num_Pair := (1, 2)`;

	function make_pair(lhs: in T; rhs: in U) return Pair;
end Pair;
