/* Constant time memmcp */

#include <stdio.h>

int memcmp_consttime(const void *s1, const void *s2, size_t n)
{
    unsigned const char *p1 = s1, *p2 = s2;
    int c = 0;
    while(n--)
        c |= (*p1++ - *p2++) & -!c;
    return c;
}

int main()
{
    printf("%d\n", memcmp_consttime("abc","abc", 3));
    printf("%d\n", memcmp_consttime("abc","ada", 3));
    printf("%d\n", memcmp_consttime("abc","aad", 3));
    return 0;
}
