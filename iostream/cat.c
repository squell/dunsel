/* Which compiler (C or C++) is the fastest; and with what options?
   Probably not what you think. */

#ifndef __cplusplus
#include <stdio.h>

int main(void)
{
    int c;
    while((c=getchar()) != EOF) 
	putchar(c);
}

#else
#include <iostream>

int main()
{
#ifdef NOSYNC
    std::ios::sync_with_stdio(false);
#endif
#ifdef NOTIE
    std::cin.tie(0);
    std::cout.tie(0);
#endif
#ifndef RDBUF
    #ifndef UNFMT
    char c;
    while(std::cin.get(c))
	std::cout << c;
    #else
    char c;
    while(std::cin.get(c))
	std::cout.put(c);
    #endif
#else
    std::cout << std::cin.rdbuf();
#endif
}

#endif
