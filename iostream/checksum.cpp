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
 *  This program calculates a dumb checksum on files by reading character by 
 *  character. Arguments: filename [buffer size]
 */

static char BUFF[0x100000]; // 1mb

unsigned short read_c(const char* fname, size_t buf_siz)
{
    unsigned short acc = 0;
    int c;
    std::FILE* f = fopen(fname, "rb");
    if(buf_siz) std::setvbuf(f, BUFF, _IOFBF, buf_siz);
    while((c=getc(f)) != -1) acc += c;
    fclose(f);
    return acc;
}

unsigned short read_opt(const char* fname, size_t const buf_siz)
{
    unsigned short acc = 0;
    int f = open(fname, O_RDONLY);
    char* ptr = BUFF;
    char* end = ptr;
    int c;
    #define nextchar() (ptr<end || ((end=BUFF+read(f,ptr=BUFF,buf_siz)) != BUFF)? *ptr++ & 0xFF : -1)
    if(buf_siz)
	while((c=nextchar()) != -1) acc += c;
    else {
	// slowest possible way of doing things
	unsigned char c;
	while(read(f,&c,1) != 0) acc += c;
    }
    close(f);
    return acc;
}

unsigned short read_sb(const char* fname, size_t const buf_siz)
{
    unsigned short acc = 0;
    std::filebuf f;
    int c;
    f.open(fname, std::ios::in | std::ios::binary);
    if(buf_siz) f.pubsetbuf(BUFF,buf_siz);
    while((c=f.sbumpc()) != -1) acc += c;
    return acc;
}

unsigned short read_strm(const char* fname, size_t const buf_siz)
{
    unsigned short acc = 0;
    std::ifstream f(fname, std::ios::in | std::ios::binary);
    if(buf_siz) f.rdbuf()->pubsetbuf(BUFF, buf_siz);
    int c;
    while((c=f.get()) != -1) acc += c;
    return acc;
}

unsigned short read_strm2(const char* fname, size_t const buf_siz)
{
    unsigned short acc = 0;
    std::ifstream f(fname, std::ios::in | std::ios::binary);
    if(buf_siz) f.rdbuf()->pubsetbuf(BUFF, buf_siz);
    char c;
    while(f.get(c)) acc += c&0xFF; // <- NOTE THIS
    return acc;
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
    size_t buf = argv[2]? std::atoi(argv[2]) : 0;
    unsigned short x;
    tstamp();
    x = read_c(argv[1], buf);
    cout << tstamp() << "regular C:" << x << endl;
    x = read_opt(argv[1], buf);
    cout << tstamp() << "posix+buf:" << x << endl;
    x = read_sb(argv[1], buf);
    cout << tstamp() << "filebufs :" << x << endl;
    x = read_strm(argv[1], buf);
    cout << tstamp() << "iostream1:" << x << endl;
    x = read_strm(argv[1], buf);
    cout << tstamp() << "iostream2:" << x << endl;
}

