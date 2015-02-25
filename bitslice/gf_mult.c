/* GF(2^8) parallel multiplication using bitslicing */

#include <string.h>
#include <stdio.h>
#include <assert.h>

typedef unsigned char byte;

#define POLY 0x169

/* straight-forward way to perform multiplication in gf2^8 */

byte gf_mul(int x, int y, int poly)
{
	byte acc = 0;
	for( ; y; x <<= 1, y >>= 1) {
		if(x > (x^poly))
			x ^= poly;
		if(y&1)
			acc ^= x;
	}
	return acc;
}

/* convert an array (of 8 bytes) to/from bitslice repr */

void slice(const byte *in, byte *out)
{
	int i, j;
	for(i=0; i<8; i++) {
		for(out[i]=j=0; j<8; j++)
			out[i] |= (in[j]>>i & 1) << j;
	}
}

/* shift 8 bytes to the left & conditional xor */

void bitslice_shift(byte *x, int poly)
{
	int i;
	byte mask = x[7];
	for(i=7; i>=1; i--) 
		x[i] = x[i-1];
	x[0] = 0;

	for(i=0; i<8; i++, poly >>= 1)
		x[i] ^= mask & -(poly&1);
}

/* multiplication loop */

void bitslice_mul(byte *x, byte *y, byte *z, int poly)
{
	int pos=0, i;
	for(i=0; i<8; i++) z[i] = 0;
	do {
		int i;
		for(i=0; i<8; i++)
			z[i] ^= x[i] & y[pos];
		if(++pos == 8) break;
		bitslice_shift(x, poly);
	} while(1);
}

void dump(const char *what, byte *buf)
{
	int i;
	printf("%s\n", what);
	for(i=0; i<8; i++) printf(":%02x", buf[i]);
	printf("\n");
}

int main(void)
{
	byte buf1[8], buf2[8], obuf[8];
	byte sbuf1[8], sbuf2[8], osbuf[8];
	int i;
  	assert(fread(buf1, 1, 8, stdin)==8);
  	assert(fread(buf2, 1, 8, stdin)==8);
	dump("input1", buf1);
	dump("input2", buf2);
	for(i=0; i<8; i++) 
		obuf[i] = gf_mul(buf1[i], buf2[i], POLY);
	dump("ref   ", obuf);

	slice(buf1, sbuf1);
	slice(buf2, sbuf2);
	bitslice_mul(sbuf1, sbuf2, osbuf, POLY);
	slice(osbuf, obuf);
	dump("sliced", obuf);
}
