First solution: "externs"
-------------------------

My first attempt at a C++ implementation was ambitious, and not entirely satisfactory. It was based on the idea of the four compass headings being unique global instances, something which has been successful in some languages, but which in this case led to pointers, then typedefs, and finally greater complexity than was really justified.



Second solution: geometric
--------------------------

My second attempt was based on vector arithmetic, and overall I was much happier with the outcome. I was also aiming to write code that was well-styled, clear in intent, and in general "orthodox" -- therefore easier to read and maintain.
