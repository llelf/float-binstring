This module contains function for formatting and parsing Double
value as C99 format string %a does.

Format is `[-]0xh.hhhhhp±ddd/`, where `h.hhhhh` is significand as a
hexadecimal floating-point number and `±ddd` is exponent as a decimal
number. Significand has as many digits as needed to exactly
represent the floating point value, fractional part can be ommitted
if not needed.

Infinity and NaN are represented as `±inf` ans `nan` accordingly.
