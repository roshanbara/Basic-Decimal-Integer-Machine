(**********Program-wise Comments**********)

(a) abs.bdim () (outputs n if n>0 else outputs 0-n)
L00 : Input n at mem[0]
L01 : mem[1] = 0
L02 : mem[2] = (mem[0]>mem[1])
L03 : if mem[2] then goto L05
L04 : mem[0] = mem[1]-mem[0]
L05 : Print mem[0]
L06 : Halt

(b) ap.bdim (computes an + n(n-1)d/2)
L00 : Input a at mem[0]
L01 : Input d at mem[1]
L02 : Input n at mem[2]
L03 : mem[3] = 2
L04 : mem[4] = 1
L05 : calculate a*n
L06 : calculate n*d
L07 : calculate n-1
L08 : calculate n-1 *(nd)
L09 : calculate n(n-1)d div 2
L10 : calculate an + n(n-1)d div 2
L11 : Print an + n(n-1)d div 2
L12 : Halt

(c) fact.bdim (computes fact(n) = n * fact(n-1))
L00 : Input n at mem[0]
L01 : mem[1] = 1
L02 : mem[2] = 1
L03 : mem[3] = 0
L04 : mem[4] = (n=0)
L05 : if mem[4] goto L09
L06 : mem[2] = n*1
L07 : calculate n-1
L08 : mem[4] = (n-1 = 0)
L09 : goto L04
L10 : Print mem[2]
L11 : Halt

(d) fib.bdim (computes f(n) = f(n-1) + f(n-2))
L00 : Input n at mem[0]
L01 : mem[1] = 0
L02 : mem[2] = 1
L03 : mem[6] = 0
L04 : mem[7] = 1
L05 : mem[3] = (n=0)
L06 : if (n=0) then goto L18
L07 : mem[3] = (n=1)
L08 : if (n=1) then goto L20
L09 : mem[6] = 1
L10 : mem[4] = fib(n-1) + fib()
L11 : mem[1] = mem[2]
L12 : mem[2] = mem[4]
L13 : mem[6] = mem[6] + 1
L14 : mem[3] = (mem[6] = n)
L15 : if(mem[6] = n) then goto L10
L16 : Print mem[4]
L17 : Halt
L18 : Print mem[1]
L17 : Halt
L18 : Print mem[2]
L19 : Halt

(e) gcd.bdim (Euiclid's GCD algorithm)
L00 : Input a at mem[0]
L01 : Input b at mem[0]
L02 : mem[4] = 0
L03 : mem[5] = (a=0)
L04 : if (a=0) then goto L15
L05 : mem[5] = (b=0)
L06 : if (a=0) then goto L13
L07 : mem[2] = a mod b
L08 : mem[0] = b
L09 : mem[1] = a mod b
L10 : mem[3] = (a mod b = 0)
L11 : mem[3] = not mem[3]
L12 : if mem[3] then goto L03
L13 : Print mem[0]
L14 : Halt
L15 : Print mem[1]
L16 : Halt

(f) reverse.bdim
L00 : Input n at mem[0]
L01 : mem[1] = 10
L02 : mem[3] = 0
L03 : mem[5] = 0
L04 : mem[2] = n mod 10
L05 : mem[5] = mem[5]*10
L06 : mem[5] = mem[5] + n mod 10
L07 : mem[0] = n div 10
L08 : mem[4] = (n=0)
L09 : mem[4] = not mem[4]
L10 : if(mem[4]) then goto L04
L10 : Print (mem[5])
L10 : Halt

(g) russian.bdim
L00 : Input m at mem[0]
L01 : Input n at mem[1]
L02 : mem[2] = 0
L03 : mem[9] = 0
L04 : mem[8] = 2
L05 : mem[3] = (m=0)
L06 : mem[4] = (n=0)
L07 : mem[5] = (m=0) OR (m=0)
L08 : if ((m=0) OR (m=0)) then goto L18
L09 : mem[6] = 2*m
L10 : mem[7] = n div 2
L11 : mem[10] = n mod 2
L12 : mem[11] = (n mod 2 = 0)
L13 : mem[13] = if (n mod 2 = 0) then goto L15
L14 : mem[9] = mem[0] + mem[9]
L15 : mem[0] = mem[6]
L16 : mem[1] = mem[7]
L17 : Goto L05
L18 : Print mem[9]
L19 : Halt

Binary Exponentiation
Pseudo-code : valid ony if x,y are integers
function modexp(x,y) {
	int ans = 1;
	while(y!=0) {
		if(2 does not divides y)
			ans = ans*x;
		y = y/2;
		x = x*x;
	}
	print ans;
}
(h) binexp.bdim
L00 : Input x at mem[0]
L01 : Input y at mem[1]
L02 : mem[2] = 1
L03 : mem[3] = 2
L04 : mem[4] = 1
L05 : mem[5] = 0
L06 : mem[6] = (y=0)
L07 : if mem[6] then goto L15
L08 : mem[7] = y mod 2
L09 : mem[8] = (y mod 2 = 0)
L10 : if mem[8] then goto L12
L11 : mem[2] = mem[2]*x
L12 : mem[1] = y div 2
L13 : mem[0] = mem[0]*mem[0]
L14 : goto L06
L15 : Print mem[2]
L16 : Halt
