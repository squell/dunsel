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

inverse() {
	lookup $1 0 1 141 246 203 82 123 209 232 79 41 192 176 225 229 199 116 180 170 75 153 43 96 95 88 63 253 204 255 64 238 178 58 110 90 241 85 77 168 201 193 10 152 21 48 68 162 194 44 69 146 108 243 57 102 66 242 53 32 111 119 187 89 25 29 254 55 103 45 49 245 105 167 100 171 19 84 37 233 9 237 92 5 202 76 36 135 191 24 62 34 240 81 236 97 23 22 94 175 211 73 166 54 67 244 71 145 223 51 147 33 59 121 183 151 133 16 181 186 60 182 112 208 6 161 250 129 130 131 126 127 128 150 115 190 86 155 158 149 217 247 2 185 164 222 106 50 109 216 138 132 114 42 20 159 136 249 220 137 154 251 124 46 195 143 184 101 72 38 200 18 74 206 231 210 98 12 224 31 239 17 117 120 113 165 142 118 61 189 188 134 87 11 40 47 163 218 212 228 15 169 39 83 4 27 252 172 230 122 7 174 99 197 219 226 234 148 139 196 213 157 248 144 107 177 13 214 235 198 14 207 173 8 78 215 227 93 80 30 179 91 35 56 52 104 70 3 140 221 156 125 160 205 26 65 28
}

# Galois field multiplications modulo AES polynomial
mul() {
	acc=0 x=$1
	for i in `seq 0 7`; do
		acc=$(( acc ^ x&-($2>>i&1) ))
		x=$(( x<<1^(0x7f - x >> 8)&0x11b ))
	done
	echo $acc
}

# very primitive blinding
ra=0
while [ $ra = 0 ]; do ra=$(od -v -A n -t u1 -N1 /dev/urandom); done
ra3=`mul $ra $ra`
ra3=$(( ra3 ^ (ra3<<1) ^ (ra3<<2) ^ (ra3<<3) ^ (ra3<<4) ))
ra3=$(( (ra3 ^ (ra3>>8))&0xff ))
ra3=`mul $ra $ra3`

blind() {
	n=$1; shift
	for x; do mul $x $n; done
}

# blinded sbox
byte_sub() {
	inv=`inverse $1`
	inv=`mul $((inv^ra)) $ra`
	affine=$(( inv ^ (inv<<1) ^ (inv<<2) ^ (inv<<3) ^ (inv<<4) ^ 0x63 ))
	affine=$(( (affine ^ (affine>>8))&0xff ))
	echo $(( ra3 ^ $(mul $affine $ra) ))
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
	echo $((`byte_sub ${14}` ^ rcon))
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
	block=`blind $ra $*`; shift 16
	key=`blind $ra $*`
	block=`map 16 xor_key $block $key`
	rcon=$ra
	for round in `seq 0 9`; do
		block=`map 16 byte_sub $(shift_rows $block)`
		[ $round = 9 ] || block=`mix_columns $block`
		key=`schedule $key $(rotate_key $key)`
		rcon=$((rcon<<1 ^ (0x7f-rcon >> 7)&0x11b))
		block=`map 16 xor_key $block $key`
	done
	map 16 inverse $(blind $ra $(map 16 inverse $block))
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
	esac | tr -cd '[0-9a-f]' | xxd -p -r | from_bytes
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

