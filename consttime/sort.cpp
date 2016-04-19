#include <limits.h>
#include <stddef.h>
#include <iterator>

/* A timing-attack resistant in-place sorting algorithm.

   Also works under the assumption that not all data accesses are equal:
   data[0] gets accessed (read and written) exactly once,
   data[1] ,,                                    ,, twice,
    ...
   data[N-1] ,,                                  ,, N times.

   And all in a predictable, non-data-dependent fashion.
 */

void secureSelectSort(int data[], size_t N)
{
    size_t i, j;
    for(i=0; i < N; i++) {
	int min = data[i];
	for(j=i+1; j < N; j++) {
	    int cur = data[j];
	    int delta = cur-min;
	    delta &= -(delta / (INT_MAX+1U));
	    data[j] = cur-delta;
	    min += delta;
	}
	data[i] = min;
    }
}

/* Abstract version, using a comparator */

template<class T, class Cmp>
void secureSelectSort(T data[], size_t N, Cmp cmp)
{
    size_t i, j;
    for(i=0; i < N; i++) {
	T min = data[i];
	for(j=i+1; j < N; j++) {
	    T cur = data[j];
	    T delta = (cur^min) & -(cmp(cur,min) / (INT_MAX+1U));
	    data[j] = cur^delta;
	    min ^= delta;;
	}
	data[i] = min;
    }
}

/* Iterator versions */

template<class Iter>
void secureSelectSort(Iter begin, Iter end)
{
    typedef typename std::iterator_traits<Iter>::value_type T;
    for(Iter p=begin; p < end; p++) {
	T min = *p;
	for(Iter q=p+1; q < end; q++) {
	    T cur = *q;
	    T delta = cur-min;
	    delta &= -(delta / (INT_MAX+1U));
	    *q = cur-delta;
	    min += delta;;
	}
	*p = min;
    }
}

template<class Iter, class Cmp>
void secureSelectSort(Iter begin, Iter end, Cmp cmp)
{
    typedef typename std::iterator_traits<Iter>::value_type T;
    for(Iter p=begin; p < end; p++) {
	T min = *p;
	for(Iter q=p+1; q < end; q++) {
	    T cur = *q;
	    T delta = (cur^min) & -(cmp(cur,min) / (INT_MAX+1U));
	    *q = cur^delta;
	    min ^= delta;;
	}
	*p = min;
    }
}

