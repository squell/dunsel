#include <cstdio>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <sys/times.h>
#include <fcntl.h>
#include <unistd.h>

/*
 *  Why iostreams suck - file input
 *  Also: why streambufs do not.
 *
 *  This program generates a large stream of zero bytes (to /dev/null)
 *  Arguments: size [buffer size]
 */

char out[] = "/dev/null";

static char BUFF[0x100000]; // 1mb

void write_c(size_t bytes, size_t buf_siz)
{
    std::FILE* f = fopen(out, "wb");
    if(buf_siz) std::setvbuf(f, BUFF, _IOFBF, buf_siz);
    int c = 0;
    while(bytes--) putc(c, f);
    fclose(f);
}

void write_opt(size_t bytes, size_t const buf_siz)
{
    int f = open(out, O_WRONLY);
    char* ptr = BUFF;
    char* const end = BUFF+buf_siz;
    int c = 0;
    #define buf_flush() (write(f, BUFF, ptr-BUFF), ptr=BUFF)
    #define nextchar(c) (*ptr++ = (c)&0xFF, ptr==end? buf_flush() : 0)
    if(buf_siz)
        while(bytes--) nextchar(c);
    else {
	// slowest possible way of doing things
	unsigned char c = 0;
	while(bytes--) write(f,&c,1);
    }
    close(f);
}

unsigned short write_sb(size_t bytes, size_t const buf_siz)
{
    std::filebuf f;
    int c = 0;
    f.open(out, std::ios::out | std::ios::binary);
    if(buf_siz) f.pubsetbuf(BUFF,buf_siz);
    while(bytes--) f.sputc(c);
}

void write_strm(size_t bytes, size_t const buf_siz)
{
    std::ofstream f(out, std::ios::out | std::ios::binary);
    if(buf_siz) f.rdbuf()->pubsetbuf(BUFF, buf_siz);
    char c = 0;
    while(bytes--) f.put(c);
}

void write_strm2(size_t bytes, size_t const buf_siz)
{
    std::ofstream f(out, std::ios::out | std::ios::binary);
    if(buf_siz) f.rdbuf()->pubsetbuf(BUFF, buf_siz);
    char c = 0;
    while(bytes--) f << c;
}

const char* tstamp()
{
    tms t;
    times(&t);
    static tms p(t);
    static char tmp[64];
    sprintf(tmp, "[+%6.3f]",(t.tms_utime-p.tms_utime)/100.0);
    p = t;
    return tmp;
}


int main(int argc, const char **argv)
{
    using namespace std;
    size_t N   = std::atoi(argv[1]);
    size_t buf = argv[2]? std::atoi(argv[2]) : 0;
    tstamp();
    write_c(N, buf);
    cout << tstamp() << "regular C" << endl;
    write_opt(N, buf);
    cout << tstamp() << "posix+buf" << endl;
    write_sb(N, buf);
    cout << tstamp() << "filebufs " << endl;
    write_strm(N, buf);
    cout << tstamp() << "iostream1" << endl;
    write_strm(N, buf);
    cout << tstamp() << "iostream2" << endl;
}

