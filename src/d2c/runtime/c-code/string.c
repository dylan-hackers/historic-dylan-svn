#include <ctype.h>

/*
as_lowercase, as_uppercase

Destructively converts the first n characters of a char buffer
to lowercase/uppercase.

The details of what actually constitutes lowercase/uppercase
is dependent on the current locale.
*/

void gd_byte_string_as_lowercase (char *cptr, int n)
{
  int i;
  for (i = 0; i < n; i++, cptr++)
  {
    *cptr = (char) tolower((int) *cptr);
  }
}

void gd_byte_string_as_uppercase (char *cptr, int n)
{
  int i;
  for (i = 0; i < n; i++, cptr++)
  {
    *cptr = (char) toupper((int) *cptr);
  }
}

