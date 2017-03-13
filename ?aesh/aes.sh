#! /bin/sh

## AESh (AES in shellscript)
##
## AES-OFB (Rijndael-128) as a POSIX-compliant (hopefully) shell script.
## Tested with bash, ksh, and dash
##
## Finally we can measure AES encryption speed in seconds per block encrypted!
##
## usage: aes.sh <password> <init-vector>
## -  this will encipher standard input in AES-OFB mode
##

## ------------------------------------------------------------------------
## Copyright (c) 2016 Marc Schoolderman
##
## Permission to use, copy, modify, and/or distribute this software for any
## purpose with or without fee is hereby granted, provided that the above
## copyright notice and this permission notice appear in all copies.
##
## THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
## WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
## MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
## ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
## WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
## ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
## OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE
## ------------------------------------------------------------------------

PWDTYPE="${3:-md5}"

# higher order bash utility function
map() {
	n="$1"; shift
	f="$1"
	for i in `seq $n`; do
		shift
		$f $*
	done
}

# used in many places
xor_key() {
	echo $((${1} ^ ${17}))
}

# s-box definition
lookup() {
	eval echo \${$(($1+2))}
}

# Galois field multiplications modulo AES polynomial
mul() {
	a=$1
	b=$2
	ret=0
	for i in `seq 8`; do
		ret=$(( ret ^ ((b & 1) * a) ))
		b=$(( b >> 1 ))
		a=$(( (a << 1) ^ (( (a << 1) >= 0x100) * 0x11b) ))
	done
	echo "$ret"
}

byte_sub() {
	# inversion (x^254 using square-multiply)
	inv=$1
	for i in `seq 6`; do
		inv=`mul $inv $inv`
		inv=`mul $inv $1`
	done
	inv=`mul $inv $inv`

	# affine transformation
	affine=$(( inv ^ (inv<<1) ^ (inv<<2) ^ (inv<<3) ^ (inv<<4) ^ 0x63 ))
	echo $(( (affine ^ (affine>>8))&0xff ))
}

# exactly what it says
shift_rows() {
	echo ${1} ${6} ${11} ${16} ${5} ${10} ${15} ${4} ${9} ${14} ${3} ${8} ${13} ${2} ${7} ${12}
}

# mix_column operation
mix_element() {
	b=$(( ($1^$2)<<1 ^ $2 ^ $3 ^ $4 ))
	echo $(( b^(0xff - b >> 8)&0x11b ))
}

mix_columns() {
	for i in 1 2 3 4; do
		map 4 mix_element $1 $2 $3 $4 $1 $2 $3 $4
		shift 4
	done
}

# keyscheduling function
rotate_key() {
	echo $((`byte_sub ${14}` ^ 1<<round ^ (round>=8?0x11b<<round-8:0) ))
	map 3 byte_sub ${15} ${16} ${13}
}

schedule() {
	w=""
	for i in 1 2 3 4; do
		w=`map 4 xor_key $* $w $w $w $w`; shift 4
		echo $w
	done
}

# 10 rounds of a not terribly complicated block cipher
aes() {
	block=`map 16 xor_key $*`; shift 16
	key="$*"
	for round in `seq 0 9`; do
		block=`map 16 byte_sub $(shift_rows $block)`
		[ $round = 9 ] || block=`mix_columns $block`
		key=`schedule $key $(rotate_key $key)`
		block=`map 16 xor_key $block $key`
	done
	echo $block
}

dump() {
	for z; do printf "$fmt" $z; done
	echo
}

# to/from binary representations
from_bytes() {
	od -v -A n -t u1 -w16
}

to_bytes() {
	for z; do printf "%02x" $z; done 
	fmt="%02x" dump $(cat) | xxd -r -p
}

# encryption in AES-OFB mode

passphrase() {
	case "$PWDTYPE" in
	hex) echo -n "$1";;
	md5) echo -n "$1" | md5sum;;
	esac | tr -cd [0-9a-f] | xxd -p -r | from_bytes
}

key=`passphrase "$1"`
iv=`passphrase "$2"`

len() {
	echo $#
}

from_bytes | while read plaintext; do
	iv=$(aes $iv $key)
	map $(len $plaintext) xor_key $iv $plaintext | to_bytes
done

