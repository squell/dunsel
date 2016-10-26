KEY="54 68 61 74 73 20 6d 79 20 4b 75 6e 67 20 46 75"
IV="54 77 6f 20 4f 6e 65 20 4e 69 6e 65 20 54 77 6f"
CIPHER="29 c3 50 5f 57 14 20 f6 40 22 99 b3 1a 02 d7 3a"

null() {
	for x in `seq "$1"`; do printf "\0"; done
}
null 16 | ./aes.sh "$KEY" "$IV" hex | od -t x1 |  grep "$CIPHER" || echo 'AESH IS BROKEN!'
