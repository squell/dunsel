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

byte_sub() {
	lookup $1 99 124 119 123 242 107 111 197 48 1 103 43 254 215 171 118 202 130 201 125 250 89 71 240 173 212 162 175 156 164 114 192 183 253 147 38 54 63 247 204 52 165 229 241 113 216 49 21 4 199 35 195 24 150 5 154 7 18 128 226 235 39 178 117 9 131 44 26 27 110 90 160 82 59 214 179 41 227 47 132 83 209 0 237 32 252 177 91 106 203 190 57 74 76 88 207 208 239 170 251 67 77 51 133 69 249 2 127 80 60 159 168 81 163 64 143 146 157 56 245 188 182 218 33 16 255 243 210 205 12 19 236 95 151 68 23 196 167 126 61 100 93 25 115 96 129 79 220 34 42 144 136 70 238 184 20 222 94 11 219 224 50 58 10 73 6 36 92 194 211 172 98 145 149 228 121 231 200 55 109 141 213 78 169 108 86 244 234 101 122 174 8 186 120 37 46 28 166 180 198 232 221 116 31 75 189 139 138 112 62 181 102 72 3 246 14 97 53 87 185 134 193 29 158 225 248 152 17 105 217 142 148 155 30 135 233 206 85 40 223 140 161 137 13 191 230 66 104 65 153 45 15 176 84 187 22
}

# exactly what it says
shift_rows() {
	echo ${1} ${6} ${11} ${16} ${5} ${10} ${15} ${4} ${9} ${14} ${3} ${8} ${13} ${2} ${7} ${12}
}

# mix_column operation
mix_element() {
	b=$(( ($1^$2)<<1 ^ $2 ^ $3 ^ $4 ))
	echo $((b >= 0x100? b^0x11b : b))
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

