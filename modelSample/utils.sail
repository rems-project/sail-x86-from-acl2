$include <prelude.sail>

val n01 : (int) -> int
function n01 (x) = loghead(1, x)

val n02 : (int) -> int
function n02 (x) = loghead(2, x)

val n03 : (int) -> int
function n03 (x) = loghead(3, x)

val n04 : (int) -> int
function n04 (x) = loghead(4, x)

val n05 : (int) -> int
function n05 (x) = loghead(5, x)

val n06 : (int) -> int
function n06 (x) = loghead(6, x)

val n08 : (int) -> int
function n08 (x) = loghead(8, x)

val n09 : (int) -> int
function n09 (x) = loghead(9, x)

val n11 : (int) -> int
function n11 (x) = loghead(11, x)

val n12 : (int) -> int
function n12 (x) = loghead(12, x)

val n16 : (int) -> int
function n16 (x) = loghead(16, x)

val n17 : (int) -> int
function n17 (x) = loghead(17, x)

val n18 : (int) -> int
function n18 (x) = loghead(18, x)

val n20 : (int) -> int
function n20 (x) = loghead(20, x)

val n21 : (int) -> int
function n21 (x) = loghead(21, x)

val n22 : (int) -> int
function n22 (x) = loghead(22, x)

val n24 : (int) -> int
function n24 (x) = loghead(24, x)

val n25 : (int) -> int
function n25 (x) = loghead(25, x)

val n26 : (int) -> int
function n26 (x) = loghead(26, x)

val n27 : (int) -> int
function n27 (x) = loghead(27, x)

val n28 : (int) -> int
function n28 (x) = loghead(28, x)

val n30 : (int) -> int
function n30 (x) = loghead(30, x)

val n32 : (int) -> int
function n32 (x) = loghead(32, x)

val n33 : (int) -> int
function n33 (x) = loghead(33, x)

val n35 : (int) -> int
function n35 (x) = loghead(35, x)

val n43 : (int) -> int
function n43 (x) = loghead(43, x)

val n44 : (int) -> int
function n44 (x) = loghead(44, x)

val n45 : (int) -> int
function n45 (x) = loghead(45, x)

val n47 : (int) -> int
function n47 (x) = loghead(47, x)

val n48 : (int) -> int
function n48 (x) = loghead(48, x)

val n49 : (int) -> int
function n49 (x) = loghead(49, x)

val n51 : (int) -> int
function n51 (x) = loghead(51, x)

val n52 : (int) -> int
function n52 (x) = loghead(52, x)

val n55 : (int) -> int
function n55 (x) = loghead(55, x)

val n59 : (int) -> int
function n59 (x) = loghead(59, x)

val n60 : (int) -> int
function n60 (x) = loghead(60, x)

val n64 : (int) -> int
function n64 (x) = loghead(64, x)

val n65 : (int) -> int
function n65 (x) = loghead(65, x)

val n80 : (int) -> int
function n80 (x) = loghead(80, x)

val n112 : (int) -> int
function n112 (x) = loghead(112, x)

val n120 : (int) -> int
function n120 (x) = loghead(120, x)

val n128 : (int) -> int
function n128 (x) = loghead(128, x)

val n256 : (int) -> int
function n256 (x) = loghead(256, x)

val n512 : (int) -> int
function n512 (x) = loghead(512, x)

val n01_to_i01 : (int) -> (int) effect {escape}
function n01_to_i01 (x) = {
	assert(0 <= x & x <= 1);
	if x < 1
	then x
	else x - 2
}

val n02_to_i02 : (int) -> (int) effect {escape}
function n02_to_i02 (x) = {
	assert(0 <= x & x <= 3);
	if x < 2
	then x
	else x - 4
}

val n03_to_i03 : (int) -> (int) effect {escape}
function n03_to_i03 (x) = {
	assert(0 <= x & x <= 7);
	if x < 4
	then x
	else x - 8
}

val n04_to_i04 : (int) -> (int) effect {escape}
function n04_to_i04 (x) = {
	assert(0 <= x & x <= 15);
	if x < 8
	then x
	else x - 16
}

val n05_to_i05 : (int) -> (int) effect {escape}
function n05_to_i05 (x) = {
	assert(0 <= x & x <= 31);
	if x < 16
	then x
	else x - 32
}

val n06_to_i06 : (int) -> (int) effect {escape}
function n06_to_i06 (x) = {
	assert(0 <= x & x <= 63);
	if x < 32
	then x
	else x - 64
}

val n08_to_i08 : (int) -> (int) effect {escape}
function n08_to_i08 (x) = {
	assert(0 <= x & x <= 255);
	if x < 128
	then x
	else x - 256
}

val n09_to_i09 : (int) -> (int) effect {escape}
function n09_to_i09 (x) = {
	assert(0 <= x & x <= 511);
	if x < 256
	then x
	else x - 512
}

val n11_to_i11 : (int) -> (int) effect {escape}
function n11_to_i11 (x) = {
	assert(0 <= x & x <= 2047);
	if x < 1024
	then x
	else x - 2048
}

val n12_to_i12 : (int) -> (int) effect {escape}
function n12_to_i12 (x) = {
	assert(0 <= x & x <= 4095);
	if x < 2048
	then x
	else x - 4096
}

val n16_to_i16 : (int) -> (int) effect {escape}
function n16_to_i16 (x) = {
	assert(0 <= x & x <= 65535);
	if x < 32768
	then x
	else x - 65536
}

val n17_to_i17 : (int) -> (int) effect {escape}
function n17_to_i17 (x) = {
	assert(0 <= x & x <= 131071);
	if x < 65536
	then x
	else x - 131072
}

val n18_to_i18 : (int) -> (int) effect {escape}
function n18_to_i18 (x) = {
	assert(0 <= x & x <= 262143);
	if x < 131072
	then x
	else x - 262144
}

val n20_to_i20 : (int) -> (int) effect {escape}
function n20_to_i20 (x) = {
	assert(0 <= x & x <= 1048575);
	if x < 524288
	then x
	else x - 1048576
}

val n21_to_i21 : (int) -> (int) effect {escape}
function n21_to_i21 (x) = {
	assert(0 <= x & x <= 2097151);
	if x < 1048576
	then x
	else x - 2097152
}

val n22_to_i22 : (int) -> (int) effect {escape}
function n22_to_i22 (x) = {
	assert(0 <= x & x <= 4194303);
	if x < 2097152
	then x
	else x - 4194304
}

val n24_to_i24 : (int) -> (int) effect {escape}
function n24_to_i24 (x) = {
	assert(0 <= x & x <= 16777215);
	if x < 8388608
	then x
	else x - 16777216
}

val n25_to_i25 : (int) -> (int) effect {escape}
function n25_to_i25 (x) = {
	assert(0 <= x & x <= 33554431);
	if x < 16777216
	then x
	else x - 33554432
}

val n26_to_i26 : (int) -> (int) effect {escape}
function n26_to_i26 (x) = {
	assert(0 <= x & x <= 67108863);
	if x < 33554432
	then x
	else x - 67108864
}

val n27_to_i27 : (int) -> (int) effect {escape}
function n27_to_i27 (x) = {
	assert(0 <= x & x <= 134217727);
	if x < 67108864
	then x
	else x - 134217728
}

val n28_to_i28 : (int) -> (int) effect {escape}
function n28_to_i28 (x) = {
	assert(0 <= x & x <= 268435455);
	if x < 134217728
	then x
	else x - 268435456
}

val n30_to_i30 : (int) -> (int) effect {escape}
function n30_to_i30 (x) = {
	assert(0 <= x & x <= 1073741823);
	if x < 536870912
	then x
	else x - 1073741824
}

val n32_to_i32 : (int) -> (int) effect {escape}
function n32_to_i32 (x) = {
	assert(0 <= x & x <= 4294967295);
	if x < 2147483648
	then x
	else x - 4294967296
}

val n33_to_i33 : (int) -> (int) effect {escape}
function n33_to_i33 (x) = {
	assert(0 <= x & x <= 8589934591);
	if x < 4294967296
	then x
	else x - 8589934592
}

val n35_to_i35 : (int) -> (int) effect {escape}
function n35_to_i35 (x) = {
	assert(0 <= x & x <= 34359738367);
	if x < 17179869184
	then x
	else x - 34359738368
}

val n43_to_i43 : (int) -> (int) effect {escape}
function n43_to_i43 (x) = {
	assert(0 <= x & x <= 8796093022207);
	if x < 4398046511104
	then x
	else x - 8796093022208
}

val n44_to_i44 : (int) -> (int) effect {escape}
function n44_to_i44 (x) = {
	assert(0 <= x & x <= 17592186044415);
	if x < 8796093022208
	then x
	else x - 17592186044416
}

val n45_to_i45 : (int) -> (int) effect {escape}
function n45_to_i45 (x) = {
	assert(0 <= x & x <= 35184372088831);
	if x < 17592186044416
	then x
	else x - 35184372088832
}

val n47_to_i47 : (int) -> (int) effect {escape}
function n47_to_i47 (x) = {
	assert(0 <= x & x <= 140737488355327);
	if x < 70368744177664
	then x
	else x - 140737488355328
}

val n48_to_i48 : (int) -> (int) effect {escape}
function n48_to_i48 (x) = {
	assert(0 <= x & x <= 281474976710655);
	if x < 140737488355328
	then x
	else x - 281474976710656
}

val n49_to_i49 : (int) -> (int) effect {escape}
function n49_to_i49 (x) = {
	assert(0 <= x & x <= 562949953421311);
	if x < 281474976710656
	then x
	else x - 562949953421312
}

val n51_to_i51 : (int) -> (int) effect {escape}
function n51_to_i51 (x) = {
	assert(0 <= x & x <= 2251799813685247);
	if x < 1125899906842624
	then x
	else x - 2251799813685248
}

val n52_to_i52 : (int) -> (int) effect {escape}
function n52_to_i52 (x) = {
	assert(0 <= x & x <= 4503599627370495);
	if x < 2251799813685248
	then x
	else x - 4503599627370496
}

val n55_to_i55 : (int) -> (int) effect {escape}
function n55_to_i55 (x) = {
	assert(0 <= x & x <= 36028797018963967);
	if x < 18014398509481984
	then x
	else x - 36028797018963968
}

val n59_to_i59 : (int) -> (int) effect {escape}
function n59_to_i59 (x) = {
	assert(0 <= x & x <= 576460752303423487);
	if x < 288230376151711744
	then x
	else x - 576460752303423488
}

val n60_to_i60 : (int) -> (int) effect {escape}
function n60_to_i60 (x) = {
	assert(0 <= x & x <= 1152921504606846975);
	if x < 576460752303423488
	then x
	else x - 1152921504606846976
}

val n64_to_i64 : (int) -> (int) effect {escape}
function n64_to_i64 (x) = {
	assert(0 <= x & x <= 18446744073709551615);
	if x < 9223372036854775808
	then x
	else x - 18446744073709551616
}

val n65_to_i65 : (int) -> (int) effect {escape}
function n65_to_i65 (x) = {
	assert(0 <= x & x <= 36893488147419103231);
	if x < 18446744073709551616
	then x
	else x - 36893488147419103232
}

val n80_to_i80 : (int) -> (int) effect {escape}
function n80_to_i80 (x) = {
	assert(0 <= x & x <= 1208925819614629174706175);
	if x < 604462909807314587353088
	then x
	else x - 1208925819614629174706176
}

val n112_to_i112 : (int) -> (int) effect {escape}
function n112_to_i112 (x) = {
	assert(0 <= x & x <= 5192296858534827628530496329220095);
	if x < 2596148429267413814265248164610048
	then x
	else x - 5192296858534827628530496329220096
}

val n120_to_i120 : (int) -> (int) effect {escape}
function n120_to_i120 (x) = {
	assert(0 <= x & x <= 1329227995784915872903807060280344575);
	if x < 664613997892457936451903530140172288
	then x
	else x - 1329227995784915872903807060280344576
}

val n128_to_i128 : (int) -> (int) effect {escape}
function n128_to_i128 (x) = {
	assert(0 <= x & x <= 340282366920938463463374607431768211455);
	if x < 170141183460469231731687303715884105728
	then x
	else x - 340282366920938463463374607431768211456
}

val n256_to_i256 : (int) -> (int) effect {escape}
function n256_to_i256 (x) = {
	assert(0 <= x & x <= 115792089237316195423570985008687907853269984665640564039457584007913129639935);
	if x < 57896044618658097711785492504343953926634992332820282019728792003956564819968
	then x
	else x - 115792089237316195423570985008687907853269984665640564039457584007913129639936
}

val n512_to_i512 : (int) -> (int) effect {escape}
function n512_to_i512 (x) = {
	assert(0 <= x & x <= 13407807929942597099574024998205846127479365820592393377723561443721764030073546976801874298166903427690031858186486050853753882811946569946433649006084095);
	if x < 6703903964971298549787012499102923063739682910296196688861780721860882015036773488400937149083451713845015929093243025426876941405973284973216824503042048
	then x
	else x - 13407807929942597099574024998205846127479365820592393377723561443721764030073546976801874298166903427690031858186486050853753882811946569946433649006084096
}

val I01 : int -> int effect {escape}
function I01 x = binary_logext (1, x)


val I02 : int -> int effect {escape}
function I02 x = binary_logext (2, x)


val I03 : int -> int effect {escape}
function I03 x = binary_logext (3, x)


val I04 : int -> int effect {escape}
function I04 x = binary_logext (4, x)


val I05 : int -> int effect {escape}
function I05 x = binary_logext (5, x)


val I06 : int -> int effect {escape}
function I06 x = binary_logext (6, x)


val I08 : int -> int effect {escape}
function I08 x = binary_logext (8, x)


val I09 : int -> int effect {escape}
function I09 x = binary_logext (9, x)


val I11 : int -> int effect {escape}
function I11 x = binary_logext (11, x)


val I12 : int -> int effect {escape}
function I12 x = binary_logext (12, x)


val I16 : int -> int effect {escape}
function I16 x = binary_logext (16, x)


val I17 : int -> int effect {escape}
function I17 x = binary_logext (17, x)


val I18 : int -> int effect {escape}
function I18 x = binary_logext (18, x)


val I20 : int -> int effect {escape}
function I20 x = binary_logext (20, x)


val I21 : int -> int effect {escape}
function I21 x = binary_logext (21, x)


val I22 : int -> int effect {escape}
function I22 x = binary_logext (22, x)


val I24 : int -> int effect {escape}
function I24 x = binary_logext (24, x)


val I25 : int -> int effect {escape}
function I25 x = binary_logext (25, x)


val I26 : int -> int effect {escape}
function I26 x = binary_logext (26, x)


val I27 : int -> int effect {escape}
function I27 x = binary_logext (27, x)


val I28 : int -> int effect {escape}
function I28 x = binary_logext (28, x)


val I30 : int -> int effect {escape}
function I30 x = binary_logext (30, x)


val I32 : int -> int effect {escape}
function I32 x = binary_logext (32, x)


val I33 : int -> int effect {escape}
function I33 x = binary_logext (33, x)


val I35 : int -> int effect {escape}
function I35 x = binary_logext (35, x)


val I43 : int -> int effect {escape}
function I43 x = binary_logext (43, x)


val I44 : int -> int effect {escape}
function I44 x = binary_logext (44, x)


val I45 : int -> int effect {escape}
function I45 x = binary_logext (45, x)


val I47 : int -> int effect {escape}
function I47 x = binary_logext (47, x)


val I48 : int -> int effect {escape}
function I48 x = binary_logext (48, x)


val I49 : int -> int effect {escape}
function I49 x = binary_logext (49, x)


val I51 : int -> int effect {escape}
function I51 x = binary_logext (51, x)


val I52 : int -> int effect {escape}
function I52 x = binary_logext (52, x)


val I55 : int -> int effect {escape}
function I55 x = binary_logext (55, x)


val I59 : int -> int effect {escape}
function I59 x = binary_logext (59, x)


val I60 : int -> int effect {escape}
function I60 x = binary_logext (60, x)


val I64 : int -> int effect {escape}
function I64 x = binary_logext (64, x)


val I65 : int -> int effect {escape}
function I65 x = binary_logext (65, x)


val I80 : int -> int effect {escape}
function I80 x = binary_logext (80, x)


val I112 : int -> int effect {escape}
function I112 x = binary_logext (112, x)


val I120 : int -> int effect {escape}
function I120 x = binary_logext (120, x)


val I128 : int -> int effect {escape}
function I128 x = binary_logext (128, x)


val I256 : int -> int effect {escape}
function I256 x = binary_logext (256, x)


val I512 : int -> int effect {escape}
function I512 x = binary_logext (512, x)
