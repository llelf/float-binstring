This module contains functions for formatting and parsing floating point
values as C99 printf/scanf functions with format string `%a` do.

Format is `[-]0xh.hhhhhp±ddd`, where `h.hhhhh` is significand as a
hexadecimal floating-point number and `±ddd` is exponent as a decimal
number. Significand has as many digits as needed to exactly
represent the floating point value, fractional part may be ommitted.

Infinity and NaN are represented as `±inf` and `nan` accordingly.

For example, `(π ∷ Double) = 0x1.921fb54442d18p+1` (*exactly*).
