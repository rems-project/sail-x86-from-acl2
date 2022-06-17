$include <prelude.sail>

val n01 : forall 'n, 'n > 0. (bits('n)) -> bits(1)
function n01 (x) = sail_mask(1, x)

val n02 : forall 'n, 'n > 0. (bits('n)) -> bits(2)
function n02 (x) = sail_mask(2, x)

val n03 : forall 'n, 'n > 0. (bits('n)) -> bits(3)
function n03 (x) = sail_mask(3, x)

val n04 : forall 'n, 'n > 0. (bits('n)) -> bits(4)
function n04 (x) = sail_mask(4, x)

val n05 : forall 'n, 'n > 0. (bits('n)) -> bits(5)
function n05 (x) = sail_mask(5, x)

val n06 : forall 'n, 'n > 0. (bits('n)) -> bits(6)
function n06 (x) = sail_mask(6, x)

val n08 : forall 'n, 'n > 0. (bits('n)) -> bits(8)
function n08 (x) = sail_mask(8, x)

val n09 : forall 'n, 'n > 0. (bits('n)) -> bits(9)
function n09 (x) = sail_mask(9, x)

val n11 : forall 'n, 'n > 0. (bits('n)) -> bits(11)
function n11 (x) = sail_mask(11, x)

val n12 : forall 'n, 'n > 0. (bits('n)) -> bits(12)
function n12 (x) = sail_mask(12, x)

val n16 : forall 'n, 'n > 0. (bits('n)) -> bits(16)
function n16 (x) = sail_mask(16, x)

val n17 : forall 'n, 'n > 0. (bits('n)) -> bits(17)
function n17 (x) = sail_mask(17, x)

val n18 : forall 'n, 'n > 0. (bits('n)) -> bits(18)
function n18 (x) = sail_mask(18, x)

val n20 : forall 'n, 'n > 0. (bits('n)) -> bits(20)
function n20 (x) = sail_mask(20, x)

val n21 : forall 'n, 'n > 0. (bits('n)) -> bits(21)
function n21 (x) = sail_mask(21, x)

val n22 : forall 'n, 'n > 0. (bits('n)) -> bits(22)
function n22 (x) = sail_mask(22, x)

val n24 : forall 'n, 'n > 0. (bits('n)) -> bits(24)
function n24 (x) = sail_mask(24, x)

val n25 : forall 'n, 'n > 0. (bits('n)) -> bits(25)
function n25 (x) = sail_mask(25, x)

val n26 : forall 'n, 'n > 0. (bits('n)) -> bits(26)
function n26 (x) = sail_mask(26, x)

val n27 : forall 'n, 'n > 0. (bits('n)) -> bits(27)
function n27 (x) = sail_mask(27, x)

val n28 : forall 'n, 'n > 0. (bits('n)) -> bits(28)
function n28 (x) = sail_mask(28, x)

val n30 : forall 'n, 'n > 0. (bits('n)) -> bits(30)
function n30 (x) = sail_mask(30, x)

val n32 : forall 'n, 'n > 0. (bits('n)) -> bits(32)
function n32 (x) = sail_mask(32, x)

val n33 : forall 'n, 'n > 0. (bits('n)) -> bits(33)
function n33 (x) = sail_mask(33, x)

val n35 : forall 'n, 'n > 0. (bits('n)) -> bits(35)
function n35 (x) = sail_mask(35, x)

val n43 : forall 'n, 'n > 0. (bits('n)) -> bits(43)
function n43 (x) = sail_mask(43, x)

val n44 : forall 'n, 'n > 0. (bits('n)) -> bits(44)
function n44 (x) = sail_mask(44, x)

val n45 : forall 'n, 'n > 0. (bits('n)) -> bits(45)
function n45 (x) = sail_mask(45, x)

val n47 : forall 'n, 'n > 0. (bits('n)) -> bits(47)
function n47 (x) = sail_mask(47, x)

val n48 : forall 'n, 'n > 0. (bits('n)) -> bits(48)
function n48 (x) = sail_mask(48, x)

val n49 : forall 'n, 'n > 0. (bits('n)) -> bits(49)
function n49 (x) = sail_mask(49, x)

val n51 : forall 'n, 'n > 0. (bits('n)) -> bits(51)
function n51 (x) = sail_mask(51, x)

val n52 : forall 'n, 'n > 0. (bits('n)) -> bits(52)
function n52 (x) = sail_mask(52, x)

val n55 : forall 'n, 'n > 0. (bits('n)) -> bits(55)
function n55 (x) = sail_mask(55, x)

val n59 : forall 'n, 'n > 0. (bits('n)) -> bits(59)
function n59 (x) = sail_mask(59, x)

val n60 : forall 'n, 'n > 0. (bits('n)) -> bits(60)
function n60 (x) = sail_mask(60, x)

val n64 : forall 'n, 'n > 0. (bits('n)) -> bits(64)
function n64 (x) = sail_mask(64, x)

val n65 : forall 'n, 'n > 0. (bits('n)) -> bits(65)
function n65 (x) = sail_mask(65, x)

val n80 : forall 'n, 'n > 0. (bits('n)) -> bits(80)
function n80 (x) = sail_mask(80, x)

val n112 : forall 'n, 'n > 0. (bits('n)) -> bits(112)
function n112 (x) = sail_mask(112, x)

val n120 : forall 'n, 'n > 0. (bits('n)) -> bits(120)
function n120 (x) = sail_mask(120, x)

val n128 : forall 'n, 'n > 0. (bits('n)) -> bits(128)
function n128 (x) = sail_mask(128, x)

val n256 : forall 'n, 'n > 0. (bits('n)) -> bits(256)
function n256 (x) = sail_mask(256, x)

val n512 : forall 'n, 'n > 0. (bits('n)) -> bits(512)
function n512 (x) = sail_mask(512, x)

val n01_to_i01 : forall 'n, 'n > 0. (bits('n)) -> sbits(1)
function n01_to_i01 (x) = sail_mask_signed(1, x)

val n02_to_i02 : forall 'n, 'n > 0. (bits('n)) -> sbits(2)
function n02_to_i02 (x) = sail_mask_signed(2, x)

val n03_to_i03 : forall 'n, 'n > 0. (bits('n)) -> sbits(3)
function n03_to_i03 (x) = sail_mask_signed(3, x)

val n04_to_i04 : forall 'n, 'n > 0. (bits('n)) -> sbits(4)
function n04_to_i04 (x) = sail_mask_signed(4, x)

val n05_to_i05 : forall 'n, 'n > 0. (bits('n)) -> sbits(5)
function n05_to_i05 (x) = sail_mask_signed(5, x)

val n06_to_i06 : forall 'n, 'n > 0. (bits('n)) -> sbits(6)
function n06_to_i06 (x) = sail_mask_signed(6, x)

val n08_to_i08 : forall 'n, 'n > 0. (bits('n)) -> sbits(8)
function n08_to_i08 (x) = sail_mask_signed(8, x)

val n09_to_i09 : forall 'n, 'n > 0. (bits('n)) -> sbits(9)
function n09_to_i09 (x) = sail_mask_signed(9, x)

val n11_to_i11 : forall 'n, 'n > 0. (bits('n)) -> sbits(11)
function n11_to_i11 (x) = sail_mask_signed(11, x)

val n12_to_i12 : forall 'n, 'n > 0. (bits('n)) -> sbits(12)
function n12_to_i12 (x) = sail_mask_signed(12, x)

val n16_to_i16 : forall 'n, 'n > 0. (bits('n)) -> sbits(16)
function n16_to_i16 (x) = sail_mask_signed(16, x)

val n17_to_i17 : forall 'n, 'n > 0. (bits('n)) -> sbits(17)
function n17_to_i17 (x) = sail_mask_signed(17, x)

val n18_to_i18 : forall 'n, 'n > 0. (bits('n)) -> sbits(18)
function n18_to_i18 (x) = sail_mask_signed(18, x)

val n20_to_i20 : forall 'n, 'n > 0. (bits('n)) -> sbits(20)
function n20_to_i20 (x) = sail_mask_signed(20, x)

val n21_to_i21 : forall 'n, 'n > 0. (bits('n)) -> sbits(21)
function n21_to_i21 (x) = sail_mask_signed(21, x)

val n22_to_i22 : forall 'n, 'n > 0. (bits('n)) -> sbits(22)
function n22_to_i22 (x) = sail_mask_signed(22, x)

val n24_to_i24 : forall 'n, 'n > 0. (bits('n)) -> sbits(24)
function n24_to_i24 (x) = sail_mask_signed(24, x)

val n25_to_i25 : forall 'n, 'n > 0. (bits('n)) -> sbits(25)
function n25_to_i25 (x) = sail_mask_signed(25, x)

val n26_to_i26 : forall 'n, 'n > 0. (bits('n)) -> sbits(26)
function n26_to_i26 (x) = sail_mask_signed(26, x)

val n27_to_i27 : forall 'n, 'n > 0. (bits('n)) -> sbits(27)
function n27_to_i27 (x) = sail_mask_signed(27, x)

val n28_to_i28 : forall 'n, 'n > 0. (bits('n)) -> sbits(28)
function n28_to_i28 (x) = sail_mask_signed(28, x)

val n30_to_i30 : forall 'n, 'n > 0. (bits('n)) -> sbits(30)
function n30_to_i30 (x) = sail_mask_signed(30, x)

val n32_to_i32 : forall 'n, 'n > 0. (bits('n)) -> sbits(32)
function n32_to_i32 (x) = sail_mask_signed(32, x)

val n33_to_i33 : forall 'n, 'n > 0. (bits('n)) -> sbits(33)
function n33_to_i33 (x) = sail_mask_signed(33, x)

val n35_to_i35 : forall 'n, 'n > 0. (bits('n)) -> sbits(35)
function n35_to_i35 (x) = sail_mask_signed(35, x)

val n43_to_i43 : forall 'n, 'n > 0. (bits('n)) -> sbits(43)
function n43_to_i43 (x) = sail_mask_signed(43, x)

val n44_to_i44 : forall 'n, 'n > 0. (bits('n)) -> sbits(44)
function n44_to_i44 (x) = sail_mask_signed(44, x)

val n45_to_i45 : forall 'n, 'n > 0. (bits('n)) -> sbits(45)
function n45_to_i45 (x) = sail_mask_signed(45, x)

val n47_to_i47 : forall 'n, 'n > 0. (bits('n)) -> sbits(47)
function n47_to_i47 (x) = sail_mask_signed(47, x)

val n48_to_i48 : forall 'n, 'n > 0. (bits('n)) -> sbits(48)
function n48_to_i48 (x) = sail_mask_signed(48, x)

val n49_to_i49 : forall 'n, 'n > 0. (bits('n)) -> sbits(49)
function n49_to_i49 (x) = sail_mask_signed(49, x)

val n51_to_i51 : forall 'n, 'n > 0. (bits('n)) -> sbits(51)
function n51_to_i51 (x) = sail_mask_signed(51, x)

val n52_to_i52 : forall 'n, 'n > 0. (bits('n)) -> sbits(52)
function n52_to_i52 (x) = sail_mask_signed(52, x)

val n55_to_i55 : forall 'n, 'n > 0. (bits('n)) -> sbits(55)
function n55_to_i55 (x) = sail_mask_signed(55, x)

val n59_to_i59 : forall 'n, 'n > 0. (bits('n)) -> sbits(59)
function n59_to_i59 (x) = sail_mask_signed(59, x)

val n60_to_i60 : forall 'n, 'n > 0. (bits('n)) -> sbits(60)
function n60_to_i60 (x) = sail_mask_signed(60, x)

val n64_to_i64 : forall 'n, 'n > 0. (bits('n)) -> sbits(64)
function n64_to_i64 (x) = sail_mask_signed(64, x)

val n65_to_i65 : forall 'n, 'n > 0. (bits('n)) -> sbits(65)
function n65_to_i65 (x) = sail_mask_signed(65, x)

val n80_to_i80 : forall 'n, 'n > 0. (bits('n)) -> sbits(80)
function n80_to_i80 (x) = sail_mask_signed(80, x)

val n112_to_i112 : forall 'n, 'n > 0. (bits('n)) -> sbits(112)
function n112_to_i112 (x) = sail_mask_signed(112, x)

val n120_to_i120 : forall 'n, 'n > 0. (bits('n)) -> sbits(120)
function n120_to_i120 (x) = sail_mask_signed(120, x)

val n128_to_i128 : forall 'n, 'n > 0. (bits('n)) -> sbits(128)
function n128_to_i128 (x) = sail_mask_signed(128, x)

val n256_to_i256 : forall 'n, 'n > 0. (bits('n)) -> sbits(256)
function n256_to_i256 (x) = sail_mask_signed(256, x)

val n512_to_i512 : forall 'n, 'n > 0. (bits('n)) -> sbits(512)
function n512_to_i512 (x) = sail_mask_signed(512, x)

val I01 : forall 'n, 'n > 0. bits('n) -> bits(1)
function I01 x = sail_mask_signed(1, x)


val I02 : forall 'n, 'n > 0. bits('n) -> bits(2)
function I02 x = sail_mask_signed(2, x)


val I03 : forall 'n, 'n > 0. bits('n) -> bits(3)
function I03 x = sail_mask_signed(3, x)


val I04 : forall 'n, 'n > 0. bits('n) -> bits(4)
function I04 x = sail_mask_signed(4, x)


val I05 : forall 'n, 'n > 0. bits('n) -> bits(5)
function I05 x = sail_mask_signed(5, x)


val I06 : forall 'n, 'n > 0. bits('n) -> bits(6)
function I06 x = sail_mask_signed(6, x)


val I08 : forall 'n, 'n > 0. bits('n) -> bits(8)
function I08 x = sail_mask_signed(8, x)


val I09 : forall 'n, 'n > 0. bits('n) -> bits(9)
function I09 x = sail_mask_signed(9, x)


val I11 : forall 'n, 'n > 0. bits('n) -> bits(11)
function I11 x = sail_mask_signed(11, x)


val I12 : forall 'n, 'n > 0. bits('n) -> bits(12)
function I12 x = sail_mask_signed(12, x)


val I16 : forall 'n, 'n > 0. bits('n) -> bits(16)
function I16 x = sail_mask_signed(16, x)


val I17 : forall 'n, 'n > 0. bits('n) -> bits(17)
function I17 x = sail_mask_signed(17, x)


val I18 : forall 'n, 'n > 0. bits('n) -> bits(18)
function I18 x = sail_mask_signed(18, x)


val I20 : forall 'n, 'n > 0. bits('n) -> bits(20)
function I20 x = sail_mask_signed(20, x)


val I21 : forall 'n, 'n > 0. bits('n) -> bits(21)
function I21 x = sail_mask_signed(21, x)


val I22 : forall 'n, 'n > 0. bits('n) -> bits(22)
function I22 x = sail_mask_signed(22, x)


val I24 : forall 'n, 'n > 0. bits('n) -> bits(24)
function I24 x = sail_mask_signed(24, x)


val I25 : forall 'n, 'n > 0. bits('n) -> bits(25)
function I25 x = sail_mask_signed(25, x)


val I26 : forall 'n, 'n > 0. bits('n) -> bits(26)
function I26 x = sail_mask_signed(26, x)


val I27 : forall 'n, 'n > 0. bits('n) -> bits(27)
function I27 x = sail_mask_signed(27, x)


val I28 : forall 'n, 'n > 0. bits('n) -> bits(28)
function I28 x = sail_mask_signed(28, x)


val I30 : forall 'n, 'n > 0. bits('n) -> bits(30)
function I30 x = sail_mask_signed(30, x)


val I32 : forall 'n, 'n > 0. bits('n) -> bits(32)
function I32 x = sail_mask_signed(32, x)


val I33 : forall 'n, 'n > 0. bits('n) -> bits(33)
function I33 x = sail_mask_signed(33, x)


val I35 : forall 'n, 'n > 0. bits('n) -> bits(35)
function I35 x = sail_mask_signed(35, x)


val I43 : forall 'n, 'n > 0. bits('n) -> bits(43)
function I43 x = sail_mask_signed(43, x)


val I44 : forall 'n, 'n > 0. bits('n) -> bits(44)
function I44 x = sail_mask_signed(44, x)


val I45 : forall 'n, 'n > 0. bits('n) -> bits(45)
function I45 x = sail_mask_signed(45, x)


val I47 : forall 'n, 'n > 0. bits('n) -> bits(47)
function I47 x = sail_mask_signed(47, x)


val I48 : forall 'n, 'n > 0. bits('n) -> bits(48)
function I48 x = sail_mask_signed(48, x)


val I49 : forall 'n, 'n > 0. bits('n) -> bits(49)
function I49 x = sail_mask_signed(49, x)


val I51 : forall 'n, 'n > 0. bits('n) -> bits(51)
function I51 x = sail_mask_signed(51, x)


val I52 : forall 'n, 'n > 0. bits('n) -> bits(52)
function I52 x = sail_mask_signed(52, x)


val I55 : forall 'n, 'n > 0. bits('n) -> bits(55)
function I55 x = sail_mask_signed(55, x)


val I59 : forall 'n, 'n > 0. bits('n) -> bits(59)
function I59 x = sail_mask_signed(59, x)


val I60 : forall 'n, 'n > 0. bits('n) -> bits(60)
function I60 x = sail_mask_signed(60, x)


val I64 : forall 'n, 'n > 0. bits('n) -> bits(64)
function I64 x = sail_mask_signed(64, x)


val I65 : forall 'n, 'n > 0. bits('n) -> bits(65)
function I65 x = sail_mask_signed(65, x)


val I80 : forall 'n, 'n > 0. bits('n) -> bits(80)
function I80 x = sail_mask_signed(80, x)


val I112 : forall 'n, 'n > 0. bits('n) -> bits(112)
function I112 x = sail_mask_signed(112, x)


val I120 : forall 'n, 'n > 0. bits('n) -> bits(120)
function I120 x = sail_mask_signed(120, x)


val I128 : forall 'n, 'n > 0. bits('n) -> bits(128)
function I128 x = sail_mask_signed(128, x)


val I256 : forall 'n, 'n > 0. bits('n) -> bits(256)
function I256 x = sail_mask_signed(256, x)


val I512 : forall 'n, 'n > 0. bits('n) -> bits(512)
function I512 x = sail_mask_signed(512, x)
