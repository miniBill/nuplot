module Glsl.Functions exposing (abs1, abs2, abs3, abs4, abs_, absd1, absd2, absd3, absd4, absf, absi1, absi2, absi3, absi4, abswd1, abswi1, acos_, acosh, asin_, asinh, atan2_, atan_, atanh, ceil, ceil1, ceil2, ceil3, ceil4, ceild1, ceild2, ceild3, ceild4, ceilf, ceilwd1, clamp111, clamp11f, clamp1f1, clamp1ff, clamp211, clamp21f, clamp222, clamp2f1, clamp2ff, clamp311, clamp31f, clamp333, clamp3f1, clamp3ff, clamp411, clamp41f, clamp444, clamp4f1, clamp4ff, clamp_, clampd1d1d1, clampd1d1wd1, clampd1wd1d1, clampd1wd1wd1, clampd2d1d1, clampd2d1wd1, clampd2d2d2, clampd2wd1d1, clampd2wd1wd1, clampd3d1d1, clampd3d1wd1, clampd3d3d3, clampd3wd1d1, clampd3wd1wd1, clampd4d1d1, clampd4d1wd1, clampd4d4d4, clampd4wd1d1, clampd4wd1wd1, clampf11, clampf1f, clampff1, clampfff, clampi1i1i1, clampi1i1wi1, clampi1wi1i1, clampi1wi1wi1, clampi2i1i1, clampi2i1wi1, clampi2i2i2, clampi2wi1i1, clampi2wi1wi1, clampi3i1i1, clampi3i1wi1, clampi3i3i3, clampi3wi1i1, clampi3wi1wi1, clampi4i1i1, clampi4i1wi1, clampi4i4i4, clampi4wi1i1, clampi4wi1wi1, clampu1u1u1, clampu1u1wu1, clampu1wu1u1, clampu1wu1wu1, clampu2u1u1, clampu2u1wu1, clampu2u2u2, clampu2wu1u1, clampu2wu1wu1, clampu3u1u1, clampu3u1wu1, clampu3u3u3, clampu3wu1u1, clampu3wu1wu1, clampu4u1u1, clampu4u1wu1, clampu4u4u4, clampu4wu1u1, clampu4wu1wu1, clampwd1d1d1, clampwd1d1wd1, clampwd1wd1d1, clampwd1wd1wd1, clampwi1i1i1, clampwi1i1wi1, clampwi1wi1i1, clampwi1wi1wi1, clampwu1u1u1, clampwu1u1wu1, clampwu1wu1u1, clampwu1wu1wu1, cos_, cosh, cross33, crossd3d3, dFdx, dFdxCoarse, dFdxFine, dFdy, dFdyCoarse, dFdyFine, degrees_, distance, distance11, distance1f, distance22, distance33, distance44, distanced1d1, distanced1wd1, distanced2d2, distanced3d3, distanced4d4, distancef1, distanceff, distancewd1d1, distancewd1wd1, dot, dot11, dot1f, dot22, dot33, dot44, dotd1d1, dotd1wd1, dotd2d2, dotd3d3, dotd4d4, dotf1, dotff, dotwd1d1, dotwd1wd1, exp, exp2, faceforward, faceforward111, faceforward11f, faceforward1f1, faceforward1ff, faceforward222, faceforward333, faceforward444, faceforwardd1d1d1, faceforwardd1d1wd1, faceforwardd1wd1d1, faceforwardd1wd1wd1, faceforwardd2d2d2, faceforwardd3d3d3, faceforwardd4d4d4, faceforwardf11, faceforwardf1f, faceforwardff1, faceforwardfff, faceforwardwd1d1d1, faceforwardwd1d1wd1, faceforwardwd1wd1d1, faceforwardwd1wd1wd1, floatBitsToInt1, floatBitsToInt2, floatBitsToInt3, floatBitsToInt4, floatBitsToIntf, floatBitsToUint1, floatBitsToUint2, floatBitsToUint3, floatBitsToUint4, floatBitsToUintf, floati1, floatwi1, floor1, floor2, floor3, floor4, floor_, floord1, floord2, floord3, floord4, floorf, floorwd1, fma, fma111, fma11f, fma1f1, fma1ff, fma222, fma333, fma444, fmad1d1d1, fmad1d1wd1, fmad1wd1d1, fmad1wd1wd1, fmad2d2d2, fmad3d3d3, fmad4d4d4, fmaf11, fmaf1f, fmaff1, fmafff, fmawd1d1d1, fmawd1d1wd1, fmawd1wd1d1, fmawd1wd1wd1, fract, fract1, fract2, fract3, fract4, fractd1, fractd2, fractd3, fractd4, fractf, fractwd1, frexp1oi1, frexp1owi1, frexp2oi2, frexp3oi3, frexp4oi4, frexpd1oi1, frexpd1owi1, frexpd2oi2, frexpd3oi3, frexpd4oi4, frexpfoi1, frexpfowi1, frexpwd1oi1, frexpwd1owi1, fwidth, fwidthCoarse, fwidthFine, int1, intBitsToFloati1, intBitsToFloati2, intBitsToFloati3, intBitsToFloati4, intBitsToFloatwi1, intf, inversesqrt, inversesqrt1, inversesqrt2, inversesqrt3, inversesqrt4, inversesqrtd1, inversesqrtd2, inversesqrtd3, inversesqrtd4, inversesqrtf, inversesqrtwd1, isinf1, isinf2, isinf3, isinf4, isinfd1, isinfd2, isinfd3, isinfd4, isinff, isinfwd1, isnan1, isnan2, isnan3, isnan4, isnand1, isnand2, isnand3, isnand4, isnanf, isnanwd1, ivec2i1i1, ivec2i1wi1, ivec2wi1i1, ivec2wi1wi1, ivec3i1i1i1, ivec3i1i1wi1, ivec3i1wi1i1, ivec3i1wi1wi1, ivec3wi1i1i1, ivec3wi1i1wi1, ivec3wi1wi1i1, ivec3wi1wi1wi1, ivec4i1i1i1i1, ivec4i1i1i1wi1, ivec4i1i1wi1i1, ivec4i1i1wi1wi1, ivec4i1wi1i1i1, ivec4i1wi1i1wi1, ivec4i1wi1wi1i1, ivec4i1wi1wi1wi1, ivec4wi1i1i1i1, ivec4wi1i1i1wi1, ivec4wi1i1wi1i1, ivec4wi1i1wi1wi1, ivec4wi1wi1i1i1, ivec4wi1wi1i1wi1, ivec4wi1wi1wi1i1, ivec4wi1wi1wi1wi1, ldexp1i1, ldexp1wi1, ldexp2i2, ldexp3i3, ldexp4i4, ldexpd1i1, ldexpd1wi1, ldexpd2i2, ldexpd3i3, ldexpd4i4, ldexpfi1, ldexpfwi1, ldexpwd1i1, ldexpwd1wi1, length, length1, length2, length3, length4, lengthd1, lengthd2, lengthd3, lengthd4, lengthf, lengthwd1, log, log2, mat21111, mat2111f, mat211f1, mat211ff, mat21f11, mat21f1f, mat21ff1, mat21fff, mat222, mat2f111, mat2f11f, mat2f1f1, mat2f1ff, mat2ff11, mat2ff1f, mat2fff1, mat2ffff, mat3111111111, mat311111111f, mat31111111f1, mat31111111ff, mat3111111f11, mat3111111f1f, mat3111111ff1, mat3111111fff, mat311111f111, mat311111f11f, mat311111f1f1, mat311111f1ff, mat311111ff11, mat311111ff1f, mat311111fff1, mat311111ffff, mat31111f1111, mat31111f111f, mat31111f11f1, mat31111f11ff, mat31111f1f11, mat31111f1f1f, mat31111f1ff1, mat31111f1fff, mat31111ff111, mat31111ff11f, mat31111ff1f1, mat31111ff1ff, mat31111fff11, mat31111fff1f, mat31111ffff1, mat31111fffff, mat3111f11111, mat3111f1111f, mat3111f111f1, mat3111f111ff, mat3111f11f11, mat3111f11f1f, mat3111f11ff1, mat3111f11fff, mat3111f1f111, mat3111f1f11f, mat3111f1f1f1, mat3111f1f1ff, mat3111f1ff11, mat3111f1ff1f, mat3111f1fff1, mat3111f1ffff, mat3111ff1111, mat3111ff111f, mat3111ff11f1, mat3111ff11ff, mat3111ff1f11, mat3111ff1f1f, mat3111ff1ff1, mat3111ff1fff, mat3111fff111, mat3111fff11f, mat3111fff1f1, mat3111fff1ff, mat3111ffff11, mat3111ffff1f, mat3111fffff1, mat3111ffffff, mat311f111111, mat311f11111f, mat311f1111f1, mat311f1111ff, mat311f111f11, mat311f111f1f, mat311f111ff1, mat311f111fff, mat311f11f111, mat311f11f11f, mat311f11f1f1, mat311f11f1ff, mat311f11ff11, mat311f11ff1f, mat311f11fff1, mat311f11ffff, mat311f1f1111, mat311f1f111f, mat311f1f11f1, mat311f1f11ff, mat311f1f1f11, mat311f1f1f1f, mat311f1f1ff1, mat311f1f1fff, mat311f1ff111, mat311f1ff11f, mat311f1ff1f1, mat311f1ff1ff, mat311f1fff11, mat311f1fff1f, mat311f1ffff1, mat311f1fffff, mat311ff11111, mat311ff1111f, mat311ff111f1, mat311ff111ff, mat311ff11f11, mat311ff11f1f, mat311ff11ff1, mat311ff11fff, mat311ff1f111, mat311ff1f11f, mat311ff1f1f1, mat311ff1f1ff, mat311ff1ff11, mat311ff1ff1f, mat311ff1fff1, mat311ff1ffff, mat311fff1111, mat311fff111f, mat311fff11f1, mat311fff11ff, mat311fff1f11, mat311fff1f1f, mat311fff1ff1, mat311fff1fff, mat311ffff111, mat311ffff11f, mat311ffff1f1, mat311ffff1ff, mat311fffff11, mat311fffff1f, mat311ffffff1, mat311fffffff, mat31f1111111, mat31f111111f, mat31f11111f1, mat31f11111ff, mat31f1111f11, mat31f1111f1f, mat31f1111ff1, mat31f1111fff, mat31f111f111, mat31f111f11f, mat31f111f1f1, mat31f111f1ff, mat31f111ff11, mat31f111ff1f, mat31f111fff1, mat31f111ffff, mat31f11f1111, mat31f11f111f, mat31f11f11f1, mat31f11f11ff, mat31f11f1f11, mat31f11f1f1f, mat31f11f1ff1, mat31f11f1fff, mat31f11ff111, mat31f11ff11f, mat31f11ff1f1, mat31f11ff1ff, mat31f11fff11, mat31f11fff1f, mat31f11ffff1, mat31f11fffff, mat31f1f11111, mat31f1f1111f, mat31f1f111f1, mat31f1f111ff, mat31f1f11f11, mat31f1f11f1f, mat31f1f11ff1, mat31f1f11fff, mat31f1f1f111, mat31f1f1f11f, mat31f1f1f1f1, mat31f1f1f1ff, mat31f1f1ff11, mat31f1f1ff1f, mat31f1f1fff1, mat31f1f1ffff, mat31f1ff1111, mat31f1ff111f, mat31f1ff11f1, mat31f1ff11ff, mat31f1ff1f11, mat31f1ff1f1f, mat31f1ff1ff1, mat31f1ff1fff, mat31f1fff111, mat31f1fff11f, mat31f1fff1f1, mat31f1fff1ff, mat31f1ffff11, mat31f1ffff1f, mat31f1fffff1, mat31f1ffffff, mat31ff111111, mat31ff11111f, mat31ff1111f1, mat31ff1111ff, mat31ff111f11, mat31ff111f1f, mat31ff111ff1, mat31ff111fff, mat31ff11f111, mat31ff11f11f, mat31ff11f1f1, mat31ff11f1ff, mat31ff11ff11, mat31ff11ff1f, mat31ff11fff1, mat31ff11ffff, mat31ff1f1111, mat31ff1f111f, mat31ff1f11f1, mat31ff1f11ff, mat31ff1f1f11, mat31ff1f1f1f, mat31ff1f1ff1, mat31ff1f1fff, mat31ff1ff111, mat31ff1ff11f, mat31ff1ff1f1, mat31ff1ff1ff, mat31ff1fff11, mat31ff1fff1f, mat31ff1ffff1, mat31ff1fffff, mat31fff11111, mat31fff1111f, mat31fff111f1, mat31fff111ff, mat31fff11f11, mat31fff11f1f, mat31fff11ff1, mat31fff11fff, mat31fff1f111, mat31fff1f11f, mat31fff1f1f1, mat31fff1f1ff, mat31fff1ff11, mat31fff1ff1f, mat31fff1fff1, mat31fff1ffff, mat31ffff1111, mat31ffff111f, mat31ffff11f1, mat31ffff11ff, mat31ffff1f11, mat31ffff1f1f, mat31ffff1ff1, mat31ffff1fff, mat31fffff111, mat31fffff11f, mat31fffff1f1, mat31fffff1ff, mat31ffffff11, mat31ffffff1f, mat31fffffff1, mat31ffffffff, mat3333, mat3f11111111, mat3f1111111f, mat3f111111f1, mat3f111111ff, mat3f11111f11, mat3f11111f1f, mat3f11111ff1, mat3f11111fff, mat3f1111f111, mat3f1111f11f, mat3f1111f1f1, mat3f1111f1ff, mat3f1111ff11, mat3f1111ff1f, mat3f1111fff1, mat3f1111ffff, mat3f111f1111, mat3f111f111f, mat3f111f11f1, mat3f111f11ff, mat3f111f1f11, mat3f111f1f1f, mat3f111f1ff1, mat3f111f1fff, mat3f111ff111, mat3f111ff11f, mat3f111ff1f1, mat3f111ff1ff, mat3f111fff11, mat3f111fff1f, mat3f111ffff1, mat3f111fffff, mat3f11f11111, mat3f11f1111f, mat3f11f111f1, mat3f11f111ff, mat3f11f11f11, mat3f11f11f1f, mat3f11f11ff1, mat3f11f11fff, mat3f11f1f111, mat3f11f1f11f, mat3f11f1f1f1, mat3f11f1f1ff, mat3f11f1ff11, mat3f11f1ff1f, mat3f11f1fff1, mat3f11f1ffff, mat3f11ff1111, mat3f11ff111f, mat3f11ff11f1, mat3f11ff11ff, mat3f11ff1f11, mat3f11ff1f1f, mat3f11ff1ff1, mat3f11ff1fff, mat3f11fff111, mat3f11fff11f, mat3f11fff1f1, mat3f11fff1ff, mat3f11ffff11, mat3f11ffff1f, mat3f11fffff1, mat3f11ffffff, mat3f1f111111, mat3f1f11111f, mat3f1f1111f1, mat3f1f1111ff, mat3f1f111f11, mat3f1f111f1f, mat3f1f111ff1, mat3f1f111fff, mat3f1f11f111, mat3f1f11f11f, mat3f1f11f1f1, mat3f1f11f1ff, mat3f1f11ff11, mat3f1f11ff1f, mat3f1f11fff1, mat3f1f11ffff, mat3f1f1f1111, mat3f1f1f111f, mat3f1f1f11f1, mat3f1f1f11ff, mat3f1f1f1f11, mat3f1f1f1f1f, mat3f1f1f1ff1, mat3f1f1f1fff, mat3f1f1ff111, mat3f1f1ff11f, mat3f1f1ff1f1, mat3f1f1ff1ff, mat3f1f1fff11, mat3f1f1fff1f, mat3f1f1ffff1, mat3f1f1fffff, mat3f1ff11111, mat3f1ff1111f, mat3f1ff111f1, mat3f1ff111ff, mat3f1ff11f11, mat3f1ff11f1f, mat3f1ff11ff1, mat3f1ff11fff, mat3f1ff1f111, mat3f1ff1f11f, mat3f1ff1f1f1, mat3f1ff1f1ff, mat3f1ff1ff11, mat3f1ff1ff1f, mat3f1ff1fff1, mat3f1ff1ffff, mat3f1fff1111, mat3f1fff111f, mat3f1fff11f1, mat3f1fff11ff, mat3f1fff1f11, mat3f1fff1f1f, mat3f1fff1ff1, mat3f1fff1fff, mat3f1ffff111, mat3f1ffff11f, mat3f1ffff1f1, mat3f1ffff1ff, mat3f1fffff11, mat3f1fffff1f, mat3f1ffffff1, mat3f1fffffff, mat3ff1111111, mat3ff111111f, mat3ff11111f1, mat3ff11111ff, mat3ff1111f11, mat3ff1111f1f, mat3ff1111ff1, mat3ff1111fff, mat3ff111f111, mat3ff111f11f, mat3ff111f1f1, mat3ff111f1ff, mat3ff111ff11, mat3ff111ff1f, mat3ff111fff1, mat3ff111ffff, mat3ff11f1111, mat3ff11f111f, mat3ff11f11f1, mat3ff11f11ff, mat3ff11f1f11, mat3ff11f1f1f, mat3ff11f1ff1, mat3ff11f1fff, mat3ff11ff111, mat3ff11ff11f, mat3ff11ff1f1, mat3ff11ff1ff, mat3ff11fff11, mat3ff11fff1f, mat3ff11ffff1, mat3ff11fffff, mat3ff1f11111, mat3ff1f1111f, mat3ff1f111f1, mat3ff1f111ff, mat3ff1f11f11, mat3ff1f11f1f, mat3ff1f11ff1, mat3ff1f11fff, mat3ff1f1f111, mat3ff1f1f11f, mat3ff1f1f1f1, mat3ff1f1f1ff, mat3ff1f1ff11, mat3ff1f1ff1f, mat3ff1f1fff1, mat3ff1f1ffff, mat3ff1ff1111, mat3ff1ff111f, mat3ff1ff11f1, mat3ff1ff11ff, mat3ff1ff1f11, mat3ff1ff1f1f, mat3ff1ff1ff1, mat3ff1ff1fff, mat3ff1fff111, mat3ff1fff11f, mat3ff1fff1f1, mat3ff1fff1ff, mat3ff1ffff11, mat3ff1ffff1f, mat3ff1fffff1, mat3ff1ffffff, mat3fff111111, mat3fff11111f, mat3fff1111f1, mat3fff1111ff, mat3fff111f11, mat3fff111f1f, mat3fff111ff1, mat3fff111fff, mat3fff11f111, mat3fff11f11f, mat3fff11f1f1, mat3fff11f1ff, mat3fff11ff11, mat3fff11ff1f, mat3fff11fff1, mat3fff11ffff, mat3fff1f1111, mat3fff1f111f, mat3fff1f11f1, mat3fff1f11ff, mat3fff1f1f11, mat3fff1f1f1f, mat3fff1f1ff1, mat3fff1f1fff, mat3fff1ff111, mat3fff1ff11f, mat3fff1ff1f1, mat3fff1ff1ff, mat3fff1fff11, mat3fff1fff1f, mat3fff1ffff1, mat3fff1fffff, mat3ffff11111, mat3ffff1111f, mat3ffff111f1, mat3ffff111ff, mat3ffff11f11, mat3ffff11f1f, mat3ffff11ff1, mat3ffff11fff, mat3ffff1f111, mat3ffff1f11f, mat3ffff1f1f1, mat3ffff1f1ff, mat3ffff1ff11, mat3ffff1ff1f, mat3ffff1fff1, mat3ffff1ffff, mat3fffff1111, mat3fffff111f, mat3fffff11f1, mat3fffff11ff, mat3fffff1f11, mat3fffff1f1f, mat3fffff1ff1, mat3fffff1fff, mat3ffffff111, mat3ffffff11f, mat3ffffff1f1, mat3ffffff1ff, mat3fffffff11, mat3fffffff1f, mat3ffffffff1, mat3fffffffff, mat44444, max11, max1f, max21, max22, max2f, max31, max33, max3f, max41, max44, max4f, max_, maxd1d1, maxd1wd1, maxd2d1, maxd2d2, maxd2wd1, maxd3d1, maxd3d3, maxd3wd1, maxd4d1, maxd4d4, maxd4wd1, maxf1, maxff, maxi1i1, maxi1wi1, maxi2i1, maxi2i2, maxi2wi1, maxi3i1, maxi3i3, maxi3wi1, maxi4i1, maxi4i4, maxi4wi1, maxu1u1, maxu1wu1, maxu2u1, maxu2u2, maxu2wu1, maxu3u1, maxu3u3, maxu3wu1, maxu4u1, maxu4u4, maxu4wu1, maxwd1d1, maxwd1wd1, maxwi1i1, maxwi1wi1, maxwu1u1, maxwu1wu1, min11, min1f, min21, min22, min2f, min31, min33, min3f, min41, min44, min4f, min_, mind1d1, mind1wd1, mind2d1, mind2d2, mind2wd1, mind3d1, mind3d3, mind3wd1, mind4d1, mind4d4, mind4wd1, minf1, minff, mini1i1, mini1wi1, mini2i1, mini2i2, mini2wi1, mini3i1, mini3i3, mini3wi1, mini4i1, mini4i4, mini4wi1, minu1u1, minu1wu1, minu2u1, minu2u2, minu2wu1, minu3u1, minu3u3, minu3wu1, minu4u1, minu4u4, minu4wu1, minwd1d1, minwd1wd1, minwi1i1, minwi1wi1, minwu1u1, minwu1wu1, mix, mix111, mix11f, mix1f1, mix1ff, mix221, mix222, mix22f, mix331, mix333, mix33f, mix441, mix444, mix44f, mixd1d1d1, mixd1d1wd1, mixd1wd1d1, mixd1wd1wd1, mixd2d2d1, mixd2d2d2, mixd2d2wd1, mixd3d3d1, mixd3d3d3, mixd3d3wd1, mixd4d4d1, mixd4d4d4, mixd4d4wd1, mixf11, mixf1f, mixff1, mixfff, mixwd1d1d1, mixwd1d1wd1, mixwd1wd1d1, mixwd1wd1wd1, mod, mod11, mod1f, mod21, mod22, mod2f, mod31, mod33, mod3f, mod41, mod44, mod4f, modd1d1, modd1wd1, modd2d1, modd2d2, modd2wd1, modd3d1, modd3d3, modd3wd1, modd4d1, modd4d4, modd4wd1, modf, modf1, modf1o1, modf1of, modf2o2, modf3o3, modf4o4, modfd1od1, modfd1owd1, modfd2od2, modfd3od3, modfd4od4, modff, modffo1, modffof, modfwd1od1, modfwd1owd1, modwd1d1, modwd1wd1, normalize, normalize1, normalize2, normalize3, normalize4, normalized1, normalized2, normalized3, normalized4, normalizef, normalizewd1, pow, radians_, reflect, reflect11, reflect1f, reflect22, reflect33, reflect44, reflectd1d1, reflectd1wd1, reflectd2d2, reflectd3d3, reflectd4d4, reflectf1, reflectff, reflectwd1d1, reflectwd1wd1, refract, refract111, refract11f, refract1f1, refract1ff, refract221, refract22f, refract331, refract33f, refract441, refract44f, refractd1d11, refractd1d1f, refractd1wd11, refractd1wd1f, refractd2d21, refractd2d2f, refractd3d31, refractd3d3f, refractd4d41, refractd4d4f, refractf11, refractf1f, refractff1, refractfff, refractwd1d11, refractwd1d1f, refractwd1wd11, refractwd1wd1f, round1, round2, round3, round4, roundEven, roundEven1, roundEven2, roundEven3, roundEven4, roundEvend1, roundEvend2, roundEvend3, roundEvend4, roundEvenf, roundEvenwd1, round_, roundd1, roundd2, roundd3, roundd4, roundf, roundwd1, sign, sign1, sign2, sign3, sign4, signd1, signd2, signd3, signd4, signf, signi1, signi2, signi3, signi4, signwd1, signwi1, sin_, sinh, smoothstep, smoothstep111, smoothstep112, smoothstep113, smoothstep114, smoothstep11f, smoothstep1f1, smoothstep1f2, smoothstep1f3, smoothstep1f4, smoothstep1ff, smoothstep222, smoothstep333, smoothstep444, smoothstepd1d1d1, smoothstepd1d1d2, smoothstepd1d1d3, smoothstepd1d1d4, smoothstepd1d1wd1, smoothstepd1wd1d1, smoothstepd1wd1d2, smoothstepd1wd1d3, smoothstepd1wd1d4, smoothstepd1wd1wd1, smoothstepd2d2d2, smoothstepd3d3d3, smoothstepd4d4d4, smoothstepf11, smoothstepf12, smoothstepf13, smoothstepf14, smoothstepf1f, smoothstepff1, smoothstepff2, smoothstepff3, smoothstepff4, smoothstepfff, smoothstepwd1d1d1, smoothstepwd1d1d2, smoothstepwd1d1d3, smoothstepwd1d1d4, smoothstepwd1d1wd1, smoothstepwd1wd1d1, smoothstepwd1wd1d2, smoothstepwd1wd1d3, smoothstepwd1wd1d4, smoothstepwd1wd1wd1, sqrt1, sqrt2, sqrt3, sqrt4, sqrt_, sqrtd1, sqrtd2, sqrtd3, sqrtd4, sqrtf, sqrtwd1, step, step11, step12, step13, step14, step1f, step22, step33, step44, stepd1d1, stepd1d2, stepd1d3, stepd1d4, stepd1wd1, stepd2d2, stepd3d3, stepd4d4, stepf1, stepf2, stepf3, stepf4, stepff, stepwd1d1, stepwd1d2, stepwd1d3, stepwd1d4, stepwd1wd1, tan_, tanh, trunc, trunc1, trunc2, trunc3, trunc4, truncd1, truncd2, truncd3, truncd4, truncf, truncwd1, uintBitsToFloatu1, uintBitsToFloatu2, uintBitsToFloatu3, uintBitsToFloatu4, uintBitsToFloatwu1, vec21, vec211, vec21f, vec21i1, vec21wi1, vec2f, vec2f1, vec2ff, vec2fi1, vec2fwi1, vec2i1, vec2i11, vec2i1f, vec2i1i1, vec2i1wi1, vec2wi1, vec2wi11, vec2wi1f, vec2wi1i1, vec2wi1wi1, vec31, vec3111, vec311f, vec311i1, vec311wi1, vec312, vec31f1, vec31ff, vec31fi1, vec31fwi1, vec31i11, vec31i1f, vec31i1i1, vec31i1wi1, vec31wi11, vec31wi1f, vec31wi1i1, vec31wi1wi1, vec321, vec32f, vec3f, vec3f11, vec3f1f, vec3f1i1, vec3f1wi1, vec3f2, vec3ff1, vec3fff, vec3ffi1, vec3ffwi1, vec3fi11, vec3fi1f, vec3fi1i1, vec3fi1wi1, vec3fwi11, vec3fwi1f, vec3fwi1i1, vec3fwi1wi1, vec3i1, vec3i111, vec3i11f, vec3i11i1, vec3i11wi1, vec3i1f1, vec3i1ff, vec3i1fi1, vec3i1fwi1, vec3i1i11, vec3i1i1f, vec3i1i1i1, vec3i1i1wi1, vec3i1wi11, vec3i1wi1f, vec3i1wi1i1, vec3i1wi1wi1, vec3wi1, vec3wi111, vec3wi11f, vec3wi11i1, vec3wi11wi1, vec3wi1f1, vec3wi1ff, vec3wi1fi1, vec3wi1fwi1, vec3wi1i11, vec3wi1i1f, vec3wi1i1i1, vec3wi1i1wi1, vec3wi1wi11, vec3wi1wi1f, vec3wi1wi1i1, vec3wi1wi1wi1, vec41, vec41111, vec4111f, vec4111i1, vec4111wi1, vec411f1, vec411ff, vec411fi1, vec411fwi1, vec411i11, vec411i1f, vec411i1i1, vec411i1wi1, vec411wi11, vec411wi1f, vec411wi1i1, vec411wi1wi1, vec413, vec41f11, vec41f1f, vec41f1i1, vec41f1wi1, vec41ff1, vec41fff, vec41ffi1, vec41ffwi1, vec41fi11, vec41fi1f, vec41fi1i1, vec41fi1wi1, vec41fwi11, vec41fwi1f, vec41fwi1i1, vec41fwi1wi1, vec41i111, vec41i11f, vec41i11i1, vec41i11wi1, vec41i1f1, vec41i1ff, vec41i1fi1, vec41i1fwi1, vec41i1i11, vec41i1i1f, vec41i1i1i1, vec41i1i1wi1, vec41i1wi11, vec41i1wi1f, vec41i1wi1i1, vec41i1wi1wi1, vec41wi111, vec41wi11f, vec41wi11i1, vec41wi11wi1, vec41wi1f1, vec41wi1ff, vec41wi1fi1, vec41wi1fwi1, vec41wi1i11, vec41wi1i1f, vec41wi1i1i1, vec41wi1i1wi1, vec41wi1wi11, vec41wi1wi1f, vec41wi1wi1i1, vec41wi1wi1wi1, vec422, vec431, vec43f, vec4f, vec4f111, vec4f11f, vec4f11i1, vec4f11wi1, vec4f1f1, vec4f1ff, vec4f1fi1, vec4f1fwi1, vec4f1i11, vec4f1i1f, vec4f1i1i1, vec4f1i1wi1, vec4f1wi11, vec4f1wi1f, vec4f1wi1i1, vec4f1wi1wi1, vec4f3, vec4ff11, vec4ff1f, vec4ff1i1, vec4ff1wi1, vec4fff1, vec4ffff, vec4fffi1, vec4fffwi1, vec4ffi11, vec4ffi1f, vec4ffi1i1, vec4ffi1wi1, vec4ffwi11, vec4ffwi1f, vec4ffwi1i1, vec4ffwi1wi1, vec4fi111, vec4fi11f, vec4fi11i1, vec4fi11wi1, vec4fi1f1, vec4fi1ff, vec4fi1fi1, vec4fi1fwi1, vec4fi1i11, vec4fi1i1f, vec4fi1i1i1, vec4fi1i1wi1, vec4fi1wi11, vec4fi1wi1f, vec4fi1wi1i1, vec4fi1wi1wi1, vec4fwi111, vec4fwi11f, vec4fwi11i1, vec4fwi11wi1, vec4fwi1f1, vec4fwi1ff, vec4fwi1fi1, vec4fwi1fwi1, vec4fwi1i11, vec4fwi1i1f, vec4fwi1i1i1, vec4fwi1i1wi1, vec4fwi1wi11, vec4fwi1wi1f, vec4fwi1wi1i1, vec4fwi1wi1wi1, vec4i1, vec4i1111, vec4i111f, vec4i111i1, vec4i111wi1, vec4i11f1, vec4i11ff, vec4i11fi1, vec4i11fwi1, vec4i11i11, vec4i11i1f, vec4i11i1i1, vec4i11i1wi1, vec4i11wi11, vec4i11wi1f, vec4i11wi1i1, vec4i11wi1wi1, vec4i1f11, vec4i1f1f, vec4i1f1i1, vec4i1f1wi1, vec4i1ff1, vec4i1fff, vec4i1ffi1, vec4i1ffwi1, vec4i1fi11, vec4i1fi1f, vec4i1fi1i1, vec4i1fi1wi1, vec4i1fwi11, vec4i1fwi1f, vec4i1fwi1i1, vec4i1fwi1wi1, vec4i1i111, vec4i1i11f, vec4i1i11i1, vec4i1i11wi1, vec4i1i1f1, vec4i1i1ff, vec4i1i1fi1, vec4i1i1fwi1, vec4i1i1i11, vec4i1i1i1f, vec4i1i1i1i1, vec4i1i1i1wi1, vec4i1i1wi11, vec4i1i1wi1f, vec4i1i1wi1i1, vec4i1i1wi1wi1, vec4i1wi111, vec4i1wi11f, vec4i1wi11i1, vec4i1wi11wi1, vec4i1wi1f1, vec4i1wi1ff, vec4i1wi1fi1, vec4i1wi1fwi1, vec4i1wi1i11, vec4i1wi1i1f, vec4i1wi1i1i1, vec4i1wi1i1wi1, vec4i1wi1wi11, vec4i1wi1wi1f, vec4i1wi1wi1i1, vec4i1wi1wi1wi1, vec4wi1, vec4wi1111, vec4wi111f, vec4wi111i1, vec4wi111wi1, vec4wi11f1, vec4wi11ff, vec4wi11fi1, vec4wi11fwi1, vec4wi11i11, vec4wi11i1f, vec4wi11i1i1, vec4wi11i1wi1, vec4wi11wi11, vec4wi11wi1f, vec4wi11wi1i1, vec4wi11wi1wi1, vec4wi1f11, vec4wi1f1f, vec4wi1f1i1, vec4wi1f1wi1, vec4wi1ff1, vec4wi1fff, vec4wi1ffi1, vec4wi1ffwi1, vec4wi1fi11, vec4wi1fi1f, vec4wi1fi1i1, vec4wi1fi1wi1, vec4wi1fwi11, vec4wi1fwi1f, vec4wi1fwi1i1, vec4wi1fwi1wi1, vec4wi1i111, vec4wi1i11f, vec4wi1i11i1, vec4wi1i11wi1, vec4wi1i1f1, vec4wi1i1ff, vec4wi1i1fi1, vec4wi1i1fwi1, vec4wi1i1i11, vec4wi1i1i1f, vec4wi1i1i1i1, vec4wi1i1i1wi1, vec4wi1i1wi11, vec4wi1i1wi1f, vec4wi1i1wi1i1, vec4wi1i1wi1wi1, vec4wi1wi111, vec4wi1wi11f, vec4wi1wi11i1, vec4wi1wi11wi1, vec4wi1wi1f1, vec4wi1wi1ff, vec4wi1wi1fi1, vec4wi1wi1fwi1, vec4wi1wi1i11, vec4wi1wi1i1f, vec4wi1wi1i1i1, vec4wi1wi1i1wi1, vec4wi1wi1wi11, vec4wi1wi1wi1f, vec4wi1wi1wi1i1, vec4wi1wi1wi1wi1)

{-|

@docs abs1, abs2, abs3, abs4, abs_, absd1, absd2, absd3, absd4, absf, absi1, absi2, absi3, absi4, abswd1, abswi1, acos_, acosh, asin_, asinh, atan2_, atan_, atanh, ceil, ceil1, ceil2, ceil3, ceil4, ceild1, ceild2, ceild3, ceild4, ceilf, ceilwd1, clamp111, clamp11f, clamp1f1, clamp1ff, clamp211, clamp21f, clamp222, clamp2f1, clamp2ff, clamp311, clamp31f, clamp333, clamp3f1, clamp3ff, clamp411, clamp41f, clamp444, clamp4f1, clamp4ff, clamp_, clampd1d1d1, clampd1d1wd1, clampd1wd1d1, clampd1wd1wd1, clampd2d1d1, clampd2d1wd1, clampd2d2d2, clampd2wd1d1, clampd2wd1wd1, clampd3d1d1, clampd3d1wd1, clampd3d3d3, clampd3wd1d1, clampd3wd1wd1, clampd4d1d1, clampd4d1wd1, clampd4d4d4, clampd4wd1d1, clampd4wd1wd1, clampf11, clampf1f, clampff1, clampfff, clampi1i1i1, clampi1i1wi1, clampi1wi1i1, clampi1wi1wi1, clampi2i1i1, clampi2i1wi1, clampi2i2i2, clampi2wi1i1, clampi2wi1wi1, clampi3i1i1, clampi3i1wi1, clampi3i3i3, clampi3wi1i1, clampi3wi1wi1, clampi4i1i1, clampi4i1wi1, clampi4i4i4, clampi4wi1i1, clampi4wi1wi1, clampu1u1u1, clampu1u1wu1, clampu1wu1u1, clampu1wu1wu1, clampu2u1u1, clampu2u1wu1, clampu2u2u2, clampu2wu1u1, clampu2wu1wu1, clampu3u1u1, clampu3u1wu1, clampu3u3u3, clampu3wu1u1, clampu3wu1wu1, clampu4u1u1, clampu4u1wu1, clampu4u4u4, clampu4wu1u1, clampu4wu1wu1, clampwd1d1d1, clampwd1d1wd1, clampwd1wd1d1, clampwd1wd1wd1, clampwi1i1i1, clampwi1i1wi1, clampwi1wi1i1, clampwi1wi1wi1, clampwu1u1u1, clampwu1u1wu1, clampwu1wu1u1, clampwu1wu1wu1, cos_, cosh, cross33, crossd3d3, dFdx, dFdxCoarse, dFdxFine, dFdy, dFdyCoarse, dFdyFine, degrees_, distance, distance11, distance1f, distance22, distance33, distance44, distanced1d1, distanced1wd1, distanced2d2, distanced3d3, distanced4d4, distancef1, distanceff, distancewd1d1, distancewd1wd1, dot, dot11, dot1f, dot22, dot33, dot44, dotd1d1, dotd1wd1, dotd2d2, dotd3d3, dotd4d4, dotf1, dotff, dotwd1d1, dotwd1wd1, exp, exp2, faceforward, faceforward111, faceforward11f, faceforward1f1, faceforward1ff, faceforward222, faceforward333, faceforward444, faceforwardd1d1d1, faceforwardd1d1wd1, faceforwardd1wd1d1, faceforwardd1wd1wd1, faceforwardd2d2d2, faceforwardd3d3d3, faceforwardd4d4d4, faceforwardf11, faceforwardf1f, faceforwardff1, faceforwardfff, faceforwardwd1d1d1, faceforwardwd1d1wd1, faceforwardwd1wd1d1, faceforwardwd1wd1wd1, floatBitsToInt1, floatBitsToInt2, floatBitsToInt3, floatBitsToInt4, floatBitsToIntf, floatBitsToUint1, floatBitsToUint2, floatBitsToUint3, floatBitsToUint4, floatBitsToUintf, floati1, floatwi1, floor1, floor2, floor3, floor4, floor_, floord1, floord2, floord3, floord4, floorf, floorwd1, fma, fma111, fma11f, fma1f1, fma1ff, fma222, fma333, fma444, fmad1d1d1, fmad1d1wd1, fmad1wd1d1, fmad1wd1wd1, fmad2d2d2, fmad3d3d3, fmad4d4d4, fmaf11, fmaf1f, fmaff1, fmafff, fmawd1d1d1, fmawd1d1wd1, fmawd1wd1d1, fmawd1wd1wd1, fract, fract1, fract2, fract3, fract4, fractd1, fractd2, fractd3, fractd4, fractf, fractwd1, frexp1oi1, frexp1owi1, frexp2oi2, frexp3oi3, frexp4oi4, frexpd1oi1, frexpd1owi1, frexpd2oi2, frexpd3oi3, frexpd4oi4, frexpfoi1, frexpfowi1, frexpwd1oi1, frexpwd1owi1, fwidth, fwidthCoarse, fwidthFine, int1, intBitsToFloati1, intBitsToFloati2, intBitsToFloati3, intBitsToFloati4, intBitsToFloatwi1, intf, inversesqrt, inversesqrt1, inversesqrt2, inversesqrt3, inversesqrt4, inversesqrtd1, inversesqrtd2, inversesqrtd3, inversesqrtd4, inversesqrtf, inversesqrtwd1, isinf1, isinf2, isinf3, isinf4, isinfd1, isinfd2, isinfd3, isinfd4, isinff, isinfwd1, isnan1, isnan2, isnan3, isnan4, isnand1, isnand2, isnand3, isnand4, isnanf, isnanwd1, ivec2i1i1, ivec2i1wi1, ivec2wi1i1, ivec2wi1wi1, ivec3i1i1i1, ivec3i1i1wi1, ivec3i1wi1i1, ivec3i1wi1wi1, ivec3wi1i1i1, ivec3wi1i1wi1, ivec3wi1wi1i1, ivec3wi1wi1wi1, ivec4i1i1i1i1, ivec4i1i1i1wi1, ivec4i1i1wi1i1, ivec4i1i1wi1wi1, ivec4i1wi1i1i1, ivec4i1wi1i1wi1, ivec4i1wi1wi1i1, ivec4i1wi1wi1wi1, ivec4wi1i1i1i1, ivec4wi1i1i1wi1, ivec4wi1i1wi1i1, ivec4wi1i1wi1wi1, ivec4wi1wi1i1i1, ivec4wi1wi1i1wi1, ivec4wi1wi1wi1i1, ivec4wi1wi1wi1wi1, ldexp1i1, ldexp1wi1, ldexp2i2, ldexp3i3, ldexp4i4, ldexpd1i1, ldexpd1wi1, ldexpd2i2, ldexpd3i3, ldexpd4i4, ldexpfi1, ldexpfwi1, ldexpwd1i1, ldexpwd1wi1, length, length1, length2, length3, length4, lengthd1, lengthd2, lengthd3, lengthd4, lengthf, lengthwd1, log, log2, mat21111, mat2111f, mat211f1, mat211ff, mat21f11, mat21f1f, mat21ff1, mat21fff, mat222, mat2f111, mat2f11f, mat2f1f1, mat2f1ff, mat2ff11, mat2ff1f, mat2fff1, mat2ffff, mat3111111111, mat311111111f, mat31111111f1, mat31111111ff, mat3111111f11, mat3111111f1f, mat3111111ff1, mat3111111fff, mat311111f111, mat311111f11f, mat311111f1f1, mat311111f1ff, mat311111ff11, mat311111ff1f, mat311111fff1, mat311111ffff, mat31111f1111, mat31111f111f, mat31111f11f1, mat31111f11ff, mat31111f1f11, mat31111f1f1f, mat31111f1ff1, mat31111f1fff, mat31111ff111, mat31111ff11f, mat31111ff1f1, mat31111ff1ff, mat31111fff11, mat31111fff1f, mat31111ffff1, mat31111fffff, mat3111f11111, mat3111f1111f, mat3111f111f1, mat3111f111ff, mat3111f11f11, mat3111f11f1f, mat3111f11ff1, mat3111f11fff, mat3111f1f111, mat3111f1f11f, mat3111f1f1f1, mat3111f1f1ff, mat3111f1ff11, mat3111f1ff1f, mat3111f1fff1, mat3111f1ffff, mat3111ff1111, mat3111ff111f, mat3111ff11f1, mat3111ff11ff, mat3111ff1f11, mat3111ff1f1f, mat3111ff1ff1, mat3111ff1fff, mat3111fff111, mat3111fff11f, mat3111fff1f1, mat3111fff1ff, mat3111ffff11, mat3111ffff1f, mat3111fffff1, mat3111ffffff, mat311f111111, mat311f11111f, mat311f1111f1, mat311f1111ff, mat311f111f11, mat311f111f1f, mat311f111ff1, mat311f111fff, mat311f11f111, mat311f11f11f, mat311f11f1f1, mat311f11f1ff, mat311f11ff11, mat311f11ff1f, mat311f11fff1, mat311f11ffff, mat311f1f1111, mat311f1f111f, mat311f1f11f1, mat311f1f11ff, mat311f1f1f11, mat311f1f1f1f, mat311f1f1ff1, mat311f1f1fff, mat311f1ff111, mat311f1ff11f, mat311f1ff1f1, mat311f1ff1ff, mat311f1fff11, mat311f1fff1f, mat311f1ffff1, mat311f1fffff, mat311ff11111, mat311ff1111f, mat311ff111f1, mat311ff111ff, mat311ff11f11, mat311ff11f1f, mat311ff11ff1, mat311ff11fff, mat311ff1f111, mat311ff1f11f, mat311ff1f1f1, mat311ff1f1ff, mat311ff1ff11, mat311ff1ff1f, mat311ff1fff1, mat311ff1ffff, mat311fff1111, mat311fff111f, mat311fff11f1, mat311fff11ff, mat311fff1f11, mat311fff1f1f, mat311fff1ff1, mat311fff1fff, mat311ffff111, mat311ffff11f, mat311ffff1f1, mat311ffff1ff, mat311fffff11, mat311fffff1f, mat311ffffff1, mat311fffffff, mat31f1111111, mat31f111111f, mat31f11111f1, mat31f11111ff, mat31f1111f11, mat31f1111f1f, mat31f1111ff1, mat31f1111fff, mat31f111f111, mat31f111f11f, mat31f111f1f1, mat31f111f1ff, mat31f111ff11, mat31f111ff1f, mat31f111fff1, mat31f111ffff, mat31f11f1111, mat31f11f111f, mat31f11f11f1, mat31f11f11ff, mat31f11f1f11, mat31f11f1f1f, mat31f11f1ff1, mat31f11f1fff, mat31f11ff111, mat31f11ff11f, mat31f11ff1f1, mat31f11ff1ff, mat31f11fff11, mat31f11fff1f, mat31f11ffff1, mat31f11fffff, mat31f1f11111, mat31f1f1111f, mat31f1f111f1, mat31f1f111ff, mat31f1f11f11, mat31f1f11f1f, mat31f1f11ff1, mat31f1f11fff, mat31f1f1f111, mat31f1f1f11f, mat31f1f1f1f1, mat31f1f1f1ff, mat31f1f1ff11, mat31f1f1ff1f, mat31f1f1fff1, mat31f1f1ffff, mat31f1ff1111, mat31f1ff111f, mat31f1ff11f1, mat31f1ff11ff, mat31f1ff1f11, mat31f1ff1f1f, mat31f1ff1ff1, mat31f1ff1fff, mat31f1fff111, mat31f1fff11f, mat31f1fff1f1, mat31f1fff1ff, mat31f1ffff11, mat31f1ffff1f, mat31f1fffff1, mat31f1ffffff, mat31ff111111, mat31ff11111f, mat31ff1111f1, mat31ff1111ff, mat31ff111f11, mat31ff111f1f, mat31ff111ff1, mat31ff111fff, mat31ff11f111, mat31ff11f11f, mat31ff11f1f1, mat31ff11f1ff, mat31ff11ff11, mat31ff11ff1f, mat31ff11fff1, mat31ff11ffff, mat31ff1f1111, mat31ff1f111f, mat31ff1f11f1, mat31ff1f11ff, mat31ff1f1f11, mat31ff1f1f1f, mat31ff1f1ff1, mat31ff1f1fff, mat31ff1ff111, mat31ff1ff11f, mat31ff1ff1f1, mat31ff1ff1ff, mat31ff1fff11, mat31ff1fff1f, mat31ff1ffff1, mat31ff1fffff, mat31fff11111, mat31fff1111f, mat31fff111f1, mat31fff111ff, mat31fff11f11, mat31fff11f1f, mat31fff11ff1, mat31fff11fff, mat31fff1f111, mat31fff1f11f, mat31fff1f1f1, mat31fff1f1ff, mat31fff1ff11, mat31fff1ff1f, mat31fff1fff1, mat31fff1ffff, mat31ffff1111, mat31ffff111f, mat31ffff11f1, mat31ffff11ff, mat31ffff1f11, mat31ffff1f1f, mat31ffff1ff1, mat31ffff1fff, mat31fffff111, mat31fffff11f, mat31fffff1f1, mat31fffff1ff, mat31ffffff11, mat31ffffff1f, mat31fffffff1, mat31ffffffff, mat3333, mat3f11111111, mat3f1111111f, mat3f111111f1, mat3f111111ff, mat3f11111f11, mat3f11111f1f, mat3f11111ff1, mat3f11111fff, mat3f1111f111, mat3f1111f11f, mat3f1111f1f1, mat3f1111f1ff, mat3f1111ff11, mat3f1111ff1f, mat3f1111fff1, mat3f1111ffff, mat3f111f1111, mat3f111f111f, mat3f111f11f1, mat3f111f11ff, mat3f111f1f11, mat3f111f1f1f, mat3f111f1ff1, mat3f111f1fff, mat3f111ff111, mat3f111ff11f, mat3f111ff1f1, mat3f111ff1ff, mat3f111fff11, mat3f111fff1f, mat3f111ffff1, mat3f111fffff, mat3f11f11111, mat3f11f1111f, mat3f11f111f1, mat3f11f111ff, mat3f11f11f11, mat3f11f11f1f, mat3f11f11ff1, mat3f11f11fff, mat3f11f1f111, mat3f11f1f11f, mat3f11f1f1f1, mat3f11f1f1ff, mat3f11f1ff11, mat3f11f1ff1f, mat3f11f1fff1, mat3f11f1ffff, mat3f11ff1111, mat3f11ff111f, mat3f11ff11f1, mat3f11ff11ff, mat3f11ff1f11, mat3f11ff1f1f, mat3f11ff1ff1, mat3f11ff1fff, mat3f11fff111, mat3f11fff11f, mat3f11fff1f1, mat3f11fff1ff, mat3f11ffff11, mat3f11ffff1f, mat3f11fffff1, mat3f11ffffff, mat3f1f111111, mat3f1f11111f, mat3f1f1111f1, mat3f1f1111ff, mat3f1f111f11, mat3f1f111f1f, mat3f1f111ff1, mat3f1f111fff, mat3f1f11f111, mat3f1f11f11f, mat3f1f11f1f1, mat3f1f11f1ff, mat3f1f11ff11, mat3f1f11ff1f, mat3f1f11fff1, mat3f1f11ffff, mat3f1f1f1111, mat3f1f1f111f, mat3f1f1f11f1, mat3f1f1f11ff, mat3f1f1f1f11, mat3f1f1f1f1f, mat3f1f1f1ff1, mat3f1f1f1fff, mat3f1f1ff111, mat3f1f1ff11f, mat3f1f1ff1f1, mat3f1f1ff1ff, mat3f1f1fff11, mat3f1f1fff1f, mat3f1f1ffff1, mat3f1f1fffff, mat3f1ff11111, mat3f1ff1111f, mat3f1ff111f1, mat3f1ff111ff, mat3f1ff11f11, mat3f1ff11f1f, mat3f1ff11ff1, mat3f1ff11fff, mat3f1ff1f111, mat3f1ff1f11f, mat3f1ff1f1f1, mat3f1ff1f1ff, mat3f1ff1ff11, mat3f1ff1ff1f, mat3f1ff1fff1, mat3f1ff1ffff, mat3f1fff1111, mat3f1fff111f, mat3f1fff11f1, mat3f1fff11ff, mat3f1fff1f11, mat3f1fff1f1f, mat3f1fff1ff1, mat3f1fff1fff, mat3f1ffff111, mat3f1ffff11f, mat3f1ffff1f1, mat3f1ffff1ff, mat3f1fffff11, mat3f1fffff1f, mat3f1ffffff1, mat3f1fffffff, mat3ff1111111, mat3ff111111f, mat3ff11111f1, mat3ff11111ff, mat3ff1111f11, mat3ff1111f1f, mat3ff1111ff1, mat3ff1111fff, mat3ff111f111, mat3ff111f11f, mat3ff111f1f1, mat3ff111f1ff, mat3ff111ff11, mat3ff111ff1f, mat3ff111fff1, mat3ff111ffff, mat3ff11f1111, mat3ff11f111f, mat3ff11f11f1, mat3ff11f11ff, mat3ff11f1f11, mat3ff11f1f1f, mat3ff11f1ff1, mat3ff11f1fff, mat3ff11ff111, mat3ff11ff11f, mat3ff11ff1f1, mat3ff11ff1ff, mat3ff11fff11, mat3ff11fff1f, mat3ff11ffff1, mat3ff11fffff, mat3ff1f11111, mat3ff1f1111f, mat3ff1f111f1, mat3ff1f111ff, mat3ff1f11f11, mat3ff1f11f1f, mat3ff1f11ff1, mat3ff1f11fff, mat3ff1f1f111, mat3ff1f1f11f, mat3ff1f1f1f1, mat3ff1f1f1ff, mat3ff1f1ff11, mat3ff1f1ff1f, mat3ff1f1fff1, mat3ff1f1ffff, mat3ff1ff1111, mat3ff1ff111f, mat3ff1ff11f1, mat3ff1ff11ff, mat3ff1ff1f11, mat3ff1ff1f1f, mat3ff1ff1ff1, mat3ff1ff1fff, mat3ff1fff111, mat3ff1fff11f, mat3ff1fff1f1, mat3ff1fff1ff, mat3ff1ffff11, mat3ff1ffff1f, mat3ff1fffff1, mat3ff1ffffff, mat3fff111111, mat3fff11111f, mat3fff1111f1, mat3fff1111ff, mat3fff111f11, mat3fff111f1f, mat3fff111ff1, mat3fff111fff, mat3fff11f111, mat3fff11f11f, mat3fff11f1f1, mat3fff11f1ff, mat3fff11ff11, mat3fff11ff1f, mat3fff11fff1, mat3fff11ffff, mat3fff1f1111, mat3fff1f111f, mat3fff1f11f1, mat3fff1f11ff, mat3fff1f1f11, mat3fff1f1f1f, mat3fff1f1ff1, mat3fff1f1fff, mat3fff1ff111, mat3fff1ff11f, mat3fff1ff1f1, mat3fff1ff1ff, mat3fff1fff11, mat3fff1fff1f, mat3fff1ffff1, mat3fff1fffff, mat3ffff11111, mat3ffff1111f, mat3ffff111f1, mat3ffff111ff, mat3ffff11f11, mat3ffff11f1f, mat3ffff11ff1, mat3ffff11fff, mat3ffff1f111, mat3ffff1f11f, mat3ffff1f1f1, mat3ffff1f1ff, mat3ffff1ff11, mat3ffff1ff1f, mat3ffff1fff1, mat3ffff1ffff, mat3fffff1111, mat3fffff111f, mat3fffff11f1, mat3fffff11ff, mat3fffff1f11, mat3fffff1f1f, mat3fffff1ff1, mat3fffff1fff, mat3ffffff111, mat3ffffff11f, mat3ffffff1f1, mat3ffffff1ff, mat3fffffff11, mat3fffffff1f, mat3ffffffff1, mat3fffffffff, mat44444, max11, max1f, max21, max22, max2f, max31, max33, max3f, max41, max44, max4f, max_, maxd1d1, maxd1wd1, maxd2d1, maxd2d2, maxd2wd1, maxd3d1, maxd3d3, maxd3wd1, maxd4d1, maxd4d4, maxd4wd1, maxf1, maxff, maxi1i1, maxi1wi1, maxi2i1, maxi2i2, maxi2wi1, maxi3i1, maxi3i3, maxi3wi1, maxi4i1, maxi4i4, maxi4wi1, maxu1u1, maxu1wu1, maxu2u1, maxu2u2, maxu2wu1, maxu3u1, maxu3u3, maxu3wu1, maxu4u1, maxu4u4, maxu4wu1, maxwd1d1, maxwd1wd1, maxwi1i1, maxwi1wi1, maxwu1u1, maxwu1wu1, min11, min1f, min21, min22, min2f, min31, min33, min3f, min41, min44, min4f, min_, mind1d1, mind1wd1, mind2d1, mind2d2, mind2wd1, mind3d1, mind3d3, mind3wd1, mind4d1, mind4d4, mind4wd1, minf1, minff, mini1i1, mini1wi1, mini2i1, mini2i2, mini2wi1, mini3i1, mini3i3, mini3wi1, mini4i1, mini4i4, mini4wi1, minu1u1, minu1wu1, minu2u1, minu2u2, minu2wu1, minu3u1, minu3u3, minu3wu1, minu4u1, minu4u4, minu4wu1, minwd1d1, minwd1wd1, minwi1i1, minwi1wi1, minwu1u1, minwu1wu1, mix, mix111, mix11f, mix1f1, mix1ff, mix221, mix222, mix22f, mix331, mix333, mix33f, mix441, mix444, mix44f, mixd1d1d1, mixd1d1wd1, mixd1wd1d1, mixd1wd1wd1, mixd2d2d1, mixd2d2d2, mixd2d2wd1, mixd3d3d1, mixd3d3d3, mixd3d3wd1, mixd4d4d1, mixd4d4d4, mixd4d4wd1, mixf11, mixf1f, mixff1, mixfff, mixwd1d1d1, mixwd1d1wd1, mixwd1wd1d1, mixwd1wd1wd1, mod, mod11, mod1f, mod21, mod22, mod2f, mod31, mod33, mod3f, mod41, mod44, mod4f, modd1d1, modd1wd1, modd2d1, modd2d2, modd2wd1, modd3d1, modd3d3, modd3wd1, modd4d1, modd4d4, modd4wd1, modf, modf1, modf1o1, modf1of, modf2o2, modf3o3, modf4o4, modfd1od1, modfd1owd1, modfd2od2, modfd3od3, modfd4od4, modff, modffo1, modffof, modfwd1od1, modfwd1owd1, modwd1d1, modwd1wd1, normalize, normalize1, normalize2, normalize3, normalize4, normalized1, normalized2, normalized3, normalized4, normalizef, normalizewd1, pow, radians_, reflect, reflect11, reflect1f, reflect22, reflect33, reflect44, reflectd1d1, reflectd1wd1, reflectd2d2, reflectd3d3, reflectd4d4, reflectf1, reflectff, reflectwd1d1, reflectwd1wd1, refract, refract111, refract11f, refract1f1, refract1ff, refract221, refract22f, refract331, refract33f, refract441, refract44f, refractd1d11, refractd1d1f, refractd1wd11, refractd1wd1f, refractd2d21, refractd2d2f, refractd3d31, refractd3d3f, refractd4d41, refractd4d4f, refractf11, refractf1f, refractff1, refractfff, refractwd1d11, refractwd1d1f, refractwd1wd11, refractwd1wd1f, round1, round2, round3, round4, roundEven, roundEven1, roundEven2, roundEven3, roundEven4, roundEvend1, roundEvend2, roundEvend3, roundEvend4, roundEvenf, roundEvenwd1, round_, roundd1, roundd2, roundd3, roundd4, roundf, roundwd1, sign, sign1, sign2, sign3, sign4, signd1, signd2, signd3, signd4, signf, signi1, signi2, signi3, signi4, signwd1, signwi1, sin_, sinh, smoothstep, smoothstep111, smoothstep112, smoothstep113, smoothstep114, smoothstep11f, smoothstep1f1, smoothstep1f2, smoothstep1f3, smoothstep1f4, smoothstep1ff, smoothstep222, smoothstep333, smoothstep444, smoothstepd1d1d1, smoothstepd1d1d2, smoothstepd1d1d3, smoothstepd1d1d4, smoothstepd1d1wd1, smoothstepd1wd1d1, smoothstepd1wd1d2, smoothstepd1wd1d3, smoothstepd1wd1d4, smoothstepd1wd1wd1, smoothstepd2d2d2, smoothstepd3d3d3, smoothstepd4d4d4, smoothstepf11, smoothstepf12, smoothstepf13, smoothstepf14, smoothstepf1f, smoothstepff1, smoothstepff2, smoothstepff3, smoothstepff4, smoothstepfff, smoothstepwd1d1d1, smoothstepwd1d1d2, smoothstepwd1d1d3, smoothstepwd1d1d4, smoothstepwd1d1wd1, smoothstepwd1wd1d1, smoothstepwd1wd1d2, smoothstepwd1wd1d3, smoothstepwd1wd1d4, smoothstepwd1wd1wd1, sqrt1, sqrt2, sqrt3, sqrt4, sqrt_, sqrtd1, sqrtd2, sqrtd3, sqrtd4, sqrtf, sqrtwd1, step, step11, step12, step13, step14, step1f, step22, step33, step44, stepd1d1, stepd1d2, stepd1d3, stepd1d4, stepd1wd1, stepd2d2, stepd3d3, stepd4d4, stepf1, stepf2, stepf3, stepf4, stepff, stepwd1d1, stepwd1d2, stepwd1d3, stepwd1d4, stepwd1wd1, tan_, tanh, trunc, trunc1, trunc2, trunc3, trunc4, truncd1, truncd2, truncd3, truncd4, truncf, truncwd1, uintBitsToFloatu1, uintBitsToFloatu2, uintBitsToFloatu3, uintBitsToFloatu4, uintBitsToFloatwu1, vec21, vec211, vec21f, vec21i1, vec21wi1, vec2f, vec2f1, vec2ff, vec2fi1, vec2fwi1, vec2i1, vec2i11, vec2i1f, vec2i1i1, vec2i1wi1, vec2wi1, vec2wi11, vec2wi1f, vec2wi1i1, vec2wi1wi1, vec31, vec3111, vec311f, vec311i1, vec311wi1, vec312, vec31f1, vec31ff, vec31fi1, vec31fwi1, vec31i11, vec31i1f, vec31i1i1, vec31i1wi1, vec31wi11, vec31wi1f, vec31wi1i1, vec31wi1wi1, vec321, vec32f, vec3f, vec3f11, vec3f1f, vec3f1i1, vec3f1wi1, vec3f2, vec3ff1, vec3fff, vec3ffi1, vec3ffwi1, vec3fi11, vec3fi1f, vec3fi1i1, vec3fi1wi1, vec3fwi11, vec3fwi1f, vec3fwi1i1, vec3fwi1wi1, vec3i1, vec3i111, vec3i11f, vec3i11i1, vec3i11wi1, vec3i1f1, vec3i1ff, vec3i1fi1, vec3i1fwi1, vec3i1i11, vec3i1i1f, vec3i1i1i1, vec3i1i1wi1, vec3i1wi11, vec3i1wi1f, vec3i1wi1i1, vec3i1wi1wi1, vec3wi1, vec3wi111, vec3wi11f, vec3wi11i1, vec3wi11wi1, vec3wi1f1, vec3wi1ff, vec3wi1fi1, vec3wi1fwi1, vec3wi1i11, vec3wi1i1f, vec3wi1i1i1, vec3wi1i1wi1, vec3wi1wi11, vec3wi1wi1f, vec3wi1wi1i1, vec3wi1wi1wi1, vec41, vec41111, vec4111f, vec4111i1, vec4111wi1, vec411f1, vec411ff, vec411fi1, vec411fwi1, vec411i11, vec411i1f, vec411i1i1, vec411i1wi1, vec411wi11, vec411wi1f, vec411wi1i1, vec411wi1wi1, vec413, vec41f11, vec41f1f, vec41f1i1, vec41f1wi1, vec41ff1, vec41fff, vec41ffi1, vec41ffwi1, vec41fi11, vec41fi1f, vec41fi1i1, vec41fi1wi1, vec41fwi11, vec41fwi1f, vec41fwi1i1, vec41fwi1wi1, vec41i111, vec41i11f, vec41i11i1, vec41i11wi1, vec41i1f1, vec41i1ff, vec41i1fi1, vec41i1fwi1, vec41i1i11, vec41i1i1f, vec41i1i1i1, vec41i1i1wi1, vec41i1wi11, vec41i1wi1f, vec41i1wi1i1, vec41i1wi1wi1, vec41wi111, vec41wi11f, vec41wi11i1, vec41wi11wi1, vec41wi1f1, vec41wi1ff, vec41wi1fi1, vec41wi1fwi1, vec41wi1i11, vec41wi1i1f, vec41wi1i1i1, vec41wi1i1wi1, vec41wi1wi11, vec41wi1wi1f, vec41wi1wi1i1, vec41wi1wi1wi1, vec422, vec431, vec43f, vec4f, vec4f111, vec4f11f, vec4f11i1, vec4f11wi1, vec4f1f1, vec4f1ff, vec4f1fi1, vec4f1fwi1, vec4f1i11, vec4f1i1f, vec4f1i1i1, vec4f1i1wi1, vec4f1wi11, vec4f1wi1f, vec4f1wi1i1, vec4f1wi1wi1, vec4f3, vec4ff11, vec4ff1f, vec4ff1i1, vec4ff1wi1, vec4fff1, vec4ffff, vec4fffi1, vec4fffwi1, vec4ffi11, vec4ffi1f, vec4ffi1i1, vec4ffi1wi1, vec4ffwi11, vec4ffwi1f, vec4ffwi1i1, vec4ffwi1wi1, vec4fi111, vec4fi11f, vec4fi11i1, vec4fi11wi1, vec4fi1f1, vec4fi1ff, vec4fi1fi1, vec4fi1fwi1, vec4fi1i11, vec4fi1i1f, vec4fi1i1i1, vec4fi1i1wi1, vec4fi1wi11, vec4fi1wi1f, vec4fi1wi1i1, vec4fi1wi1wi1, vec4fwi111, vec4fwi11f, vec4fwi11i1, vec4fwi11wi1, vec4fwi1f1, vec4fwi1ff, vec4fwi1fi1, vec4fwi1fwi1, vec4fwi1i11, vec4fwi1i1f, vec4fwi1i1i1, vec4fwi1i1wi1, vec4fwi1wi11, vec4fwi1wi1f, vec4fwi1wi1i1, vec4fwi1wi1wi1, vec4i1, vec4i1111, vec4i111f, vec4i111i1, vec4i111wi1, vec4i11f1, vec4i11ff, vec4i11fi1, vec4i11fwi1, vec4i11i11, vec4i11i1f, vec4i11i1i1, vec4i11i1wi1, vec4i11wi11, vec4i11wi1f, vec4i11wi1i1, vec4i11wi1wi1, vec4i1f11, vec4i1f1f, vec4i1f1i1, vec4i1f1wi1, vec4i1ff1, vec4i1fff, vec4i1ffi1, vec4i1ffwi1, vec4i1fi11, vec4i1fi1f, vec4i1fi1i1, vec4i1fi1wi1, vec4i1fwi11, vec4i1fwi1f, vec4i1fwi1i1, vec4i1fwi1wi1, vec4i1i111, vec4i1i11f, vec4i1i11i1, vec4i1i11wi1, vec4i1i1f1, vec4i1i1ff, vec4i1i1fi1, vec4i1i1fwi1, vec4i1i1i11, vec4i1i1i1f, vec4i1i1i1i1, vec4i1i1i1wi1, vec4i1i1wi11, vec4i1i1wi1f, vec4i1i1wi1i1, vec4i1i1wi1wi1, vec4i1wi111, vec4i1wi11f, vec4i1wi11i1, vec4i1wi11wi1, vec4i1wi1f1, vec4i1wi1ff, vec4i1wi1fi1, vec4i1wi1fwi1, vec4i1wi1i11, vec4i1wi1i1f, vec4i1wi1i1i1, vec4i1wi1i1wi1, vec4i1wi1wi11, vec4i1wi1wi1f, vec4i1wi1wi1i1, vec4i1wi1wi1wi1, vec4wi1, vec4wi1111, vec4wi111f, vec4wi111i1, vec4wi111wi1, vec4wi11f1, vec4wi11ff, vec4wi11fi1, vec4wi11fwi1, vec4wi11i11, vec4wi11i1f, vec4wi11i1i1, vec4wi11i1wi1, vec4wi11wi11, vec4wi11wi1f, vec4wi11wi1i1, vec4wi11wi1wi1, vec4wi1f11, vec4wi1f1f, vec4wi1f1i1, vec4wi1f1wi1, vec4wi1ff1, vec4wi1fff, vec4wi1ffi1, vec4wi1ffwi1, vec4wi1fi11, vec4wi1fi1f, vec4wi1fi1i1, vec4wi1fi1wi1, vec4wi1fwi11, vec4wi1fwi1f, vec4wi1fwi1i1, vec4wi1fwi1wi1, vec4wi1i111, vec4wi1i11f, vec4wi1i11i1, vec4wi1i11wi1, vec4wi1i1f1, vec4wi1i1ff, vec4wi1i1fi1, vec4wi1i1fwi1, vec4wi1i1i11, vec4wi1i1i1f, vec4wi1i1i1i1, vec4wi1i1i1wi1, vec4wi1i1wi11, vec4wi1i1wi1f, vec4wi1i1wi1i1, vec4wi1i1wi1wi1, vec4wi1wi111, vec4wi1wi11f, vec4wi1wi11i1, vec4wi1wi11wi1, vec4wi1wi1f1, vec4wi1wi1ff, vec4wi1wi1fi1, vec4wi1wi1fwi1, vec4wi1wi1i11, vec4wi1wi1i1f, vec4wi1wi1i1i1, vec4wi1wi1i1wi1, vec4wi1wi1wi11, vec4wi1wi1wi1f, vec4wi1wi1wi1i1, vec4wi1wi1wi1wi1

-}

import Glsl


abs1 : Glsl.Expression Glsl.Float_ -> Glsl.Expression Glsl.Float_
abs1 a =
    Glsl.unsafeCall1 "abs" [] a


abs2 : Glsl.Expression Glsl.Vec2 -> Glsl.Expression Glsl.Vec2
abs2 a =
    Glsl.unsafeCall1 "abs" [] a


abs3 : Glsl.Expression Glsl.Vec3 -> Glsl.Expression Glsl.Vec3
abs3 a =
    Glsl.unsafeCall1 "abs" [] a


abs4 : Glsl.Expression Glsl.Vec4 -> Glsl.Expression Glsl.Vec4
abs4 a =
    Glsl.unsafeCall1 "abs" [] a


absd1 : Glsl.Expression Glsl.Double -> Glsl.Expression Glsl.Double
absd1 a =
    Glsl.unsafeCall1 "abs" [] a


absd2 : Glsl.Expression Glsl.DVec2 -> Glsl.Expression Glsl.DVec2
absd2 a =
    Glsl.unsafeCall1 "abs" [] a


absd3 : Glsl.Expression Glsl.DVec3 -> Glsl.Expression Glsl.DVec3
absd3 a =
    Glsl.unsafeCall1 "abs" [] a


absd4 : Glsl.Expression Glsl.DVec4 -> Glsl.Expression Glsl.DVec4
absd4 a =
    Glsl.unsafeCall1 "abs" [] a


absf : Float -> Glsl.Expression Glsl.Float_
absf a =
    Glsl.unsafeCall1 "abs" [] (Glsl.float1 a)


absi1 : Glsl.Expression Glsl.Int_ -> Glsl.Expression Glsl.Int_
absi1 a =
    Glsl.unsafeCall1 "abs" [] a


absi2 : Glsl.Expression Glsl.IVec2 -> Glsl.Expression Glsl.IVec2
absi2 a =
    Glsl.unsafeCall1 "abs" [] a


absi3 : Glsl.Expression Glsl.IVec3 -> Glsl.Expression Glsl.IVec3
absi3 a =
    Glsl.unsafeCall1 "abs" [] a


absi4 : Glsl.Expression Glsl.IVec4 -> Glsl.Expression Glsl.IVec4
absi4 a =
    Glsl.unsafeCall1 "abs" [] a


abswd1 : Float -> Glsl.Expression Glsl.Double
abswd1 a =
    Glsl.unsafeCall1 "abs" [] (Glsl.double1 a)


abswi1 : Int -> Glsl.Expression Glsl.Int_
abswi1 a =
    Glsl.unsafeCall1 "abs" [] (Glsl.int1 a)


ceil1 : Glsl.Expression Glsl.Float_ -> Glsl.Expression Glsl.Float_
ceil1 a =
    Glsl.unsafeCall1 "ceil" [] a


ceil2 : Glsl.Expression Glsl.Vec2 -> Glsl.Expression Glsl.Vec2
ceil2 a =
    Glsl.unsafeCall1 "ceil" [] a


ceil3 : Glsl.Expression Glsl.Vec3 -> Glsl.Expression Glsl.Vec3
ceil3 a =
    Glsl.unsafeCall1 "ceil" [] a


ceil4 : Glsl.Expression Glsl.Vec4 -> Glsl.Expression Glsl.Vec4
ceil4 a =
    Glsl.unsafeCall1 "ceil" [] a


ceild1 : Glsl.Expression Glsl.Double -> Glsl.Expression Glsl.Double
ceild1 a =
    Glsl.unsafeCall1 "ceil" [] a


ceild2 : Glsl.Expression Glsl.DVec2 -> Glsl.Expression Glsl.DVec2
ceild2 a =
    Glsl.unsafeCall1 "ceil" [] a


ceild3 : Glsl.Expression Glsl.DVec3 -> Glsl.Expression Glsl.DVec3
ceild3 a =
    Glsl.unsafeCall1 "ceil" [] a


ceild4 : Glsl.Expression Glsl.DVec4 -> Glsl.Expression Glsl.DVec4
ceild4 a =
    Glsl.unsafeCall1 "ceil" [] a


ceilf : Float -> Glsl.Expression Glsl.Float_
ceilf a =
    Glsl.unsafeCall1 "ceil" [] (Glsl.float1 a)


ceilwd1 : Float -> Glsl.Expression Glsl.Double
ceilwd1 a =
    Glsl.unsafeCall1 "ceil" [] (Glsl.double1 a)


ceil : Glsl.Expression (Glsl.Vec t a) -> Glsl.Expression (Glsl.Vec t a)
ceil a =
    Glsl.unsafeCall1 "ceil" [] a


clamp111 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
clamp111 a b c =
    Glsl.unsafeCall3 "clamp" [] a b c


clamp11f :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
clamp11f a b c =
    Glsl.unsafeCall3 "clamp" [] a b (Glsl.float1 c)


clamp1f1 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
clamp1f1 a b c =
    Glsl.unsafeCall3 "clamp" [] a (Glsl.float1 b) c


clamp1ff : Glsl.Expression Glsl.Float_ -> Float -> Float -> Glsl.Expression Glsl.Float_
clamp1ff a b c =
    Glsl.unsafeCall3 "clamp" [] a (Glsl.float1 b) (Glsl.float1 c)


clamp211 :
    Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec2
clamp211 a b c =
    Glsl.unsafeCall3 "clamp" [] a b c


clamp21f :
    Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Vec2
clamp21f a b c =
    Glsl.unsafeCall3 "clamp" [] a b (Glsl.float1 c)


clamp222 :
    Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
clamp222 a b c =
    Glsl.unsafeCall3 "clamp" [] a b c


clamp2f1 :
    Glsl.Expression Glsl.Vec2
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec2
clamp2f1 a b c =
    Glsl.unsafeCall3 "clamp" [] a (Glsl.float1 b) c


clamp2ff : Glsl.Expression Glsl.Vec2 -> Float -> Float -> Glsl.Expression Glsl.Vec2
clamp2ff a b c =
    Glsl.unsafeCall3 "clamp" [] a (Glsl.float1 b) (Glsl.float1 c)


clamp311 :
    Glsl.Expression Glsl.Vec3
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec3
clamp311 a b c =
    Glsl.unsafeCall3 "clamp" [] a b c


clamp31f :
    Glsl.Expression Glsl.Vec3
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Vec3
clamp31f a b c =
    Glsl.unsafeCall3 "clamp" [] a b (Glsl.float1 c)


clamp333 :
    Glsl.Expression Glsl.Vec3
    -> Glsl.Expression Glsl.Vec3
    -> Glsl.Expression Glsl.Vec3
    -> Glsl.Expression Glsl.Vec3
clamp333 a b c =
    Glsl.unsafeCall3 "clamp" [] a b c


clamp3f1 :
    Glsl.Expression Glsl.Vec3
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec3
clamp3f1 a b c =
    Glsl.unsafeCall3 "clamp" [] a (Glsl.float1 b) c


clamp3ff : Glsl.Expression Glsl.Vec3 -> Float -> Float -> Glsl.Expression Glsl.Vec3
clamp3ff a b c =
    Glsl.unsafeCall3 "clamp" [] a (Glsl.float1 b) (Glsl.float1 c)


clamp411 :
    Glsl.Expression Glsl.Vec4
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
clamp411 a b c =
    Glsl.unsafeCall3 "clamp" [] a b c


clamp41f :
    Glsl.Expression Glsl.Vec4
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Vec4
clamp41f a b c =
    Glsl.unsafeCall3 "clamp" [] a b (Glsl.float1 c)


clamp444 :
    Glsl.Expression Glsl.Vec4
    -> Glsl.Expression Glsl.Vec4
    -> Glsl.Expression Glsl.Vec4
    -> Glsl.Expression Glsl.Vec4
clamp444 a b c =
    Glsl.unsafeCall3 "clamp" [] a b c


clamp4f1 :
    Glsl.Expression Glsl.Vec4
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
clamp4f1 a b c =
    Glsl.unsafeCall3 "clamp" [] a (Glsl.float1 b) c


clamp4ff : Glsl.Expression Glsl.Vec4 -> Float -> Float -> Glsl.Expression Glsl.Vec4
clamp4ff a b c =
    Glsl.unsafeCall3 "clamp" [] a (Glsl.float1 b) (Glsl.float1 c)


clampd1d1d1 :
    Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.Double
clampd1d1d1 a b c =
    Glsl.unsafeCall3 "clamp" [] a b c


clampd1d1wd1 :
    Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.Double
    -> Float
    -> Glsl.Expression Glsl.Double
clampd1d1wd1 a b c =
    Glsl.unsafeCall3 "clamp" [] a b (Glsl.double1 c)


clampd1wd1d1 :
    Glsl.Expression Glsl.Double
    -> Float
    -> Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.Double
clampd1wd1d1 a b c =
    Glsl.unsafeCall3 "clamp" [] a (Glsl.double1 b) c


clampd1wd1wd1 : Glsl.Expression Glsl.Double -> Float -> Float -> Glsl.Expression Glsl.Double
clampd1wd1wd1 a b c =
    Glsl.unsafeCall3 "clamp" [] a (Glsl.double1 b) (Glsl.double1 c)


clampd2d1d1 :
    Glsl.Expression Glsl.DVec2
    -> Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.DVec2
clampd2d1d1 a b c =
    Glsl.unsafeCall3 "clamp" [] a b c


clampd2d1wd1 :
    Glsl.Expression Glsl.DVec2
    -> Glsl.Expression Glsl.Double
    -> Float
    -> Glsl.Expression Glsl.DVec2
clampd2d1wd1 a b c =
    Glsl.unsafeCall3 "clamp" [] a b (Glsl.double1 c)


clampd2d2d2 :
    Glsl.Expression Glsl.DVec2
    -> Glsl.Expression Glsl.DVec2
    -> Glsl.Expression Glsl.DVec2
    -> Glsl.Expression Glsl.DVec2
clampd2d2d2 a b c =
    Glsl.unsafeCall3 "clamp" [] a b c


clampd2wd1d1 :
    Glsl.Expression Glsl.DVec2
    -> Float
    -> Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.DVec2
clampd2wd1d1 a b c =
    Glsl.unsafeCall3 "clamp" [] a (Glsl.double1 b) c


clampd2wd1wd1 : Glsl.Expression Glsl.DVec2 -> Float -> Float -> Glsl.Expression Glsl.DVec2
clampd2wd1wd1 a b c =
    Glsl.unsafeCall3 "clamp" [] a (Glsl.double1 b) (Glsl.double1 c)


clampd3d1d1 :
    Glsl.Expression Glsl.DVec3
    -> Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.DVec3
clampd3d1d1 a b c =
    Glsl.unsafeCall3 "clamp" [] a b c


clampd3d1wd1 :
    Glsl.Expression Glsl.DVec3
    -> Glsl.Expression Glsl.Double
    -> Float
    -> Glsl.Expression Glsl.DVec3
clampd3d1wd1 a b c =
    Glsl.unsafeCall3 "clamp" [] a b (Glsl.double1 c)


clampd3d3d3 :
    Glsl.Expression Glsl.DVec3
    -> Glsl.Expression Glsl.DVec3
    -> Glsl.Expression Glsl.DVec3
    -> Glsl.Expression Glsl.DVec3
clampd3d3d3 a b c =
    Glsl.unsafeCall3 "clamp" [] a b c


clampd3wd1d1 :
    Glsl.Expression Glsl.DVec3
    -> Float
    -> Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.DVec3
clampd3wd1d1 a b c =
    Glsl.unsafeCall3 "clamp" [] a (Glsl.double1 b) c


clampd3wd1wd1 : Glsl.Expression Glsl.DVec3 -> Float -> Float -> Glsl.Expression Glsl.DVec3
clampd3wd1wd1 a b c =
    Glsl.unsafeCall3 "clamp" [] a (Glsl.double1 b) (Glsl.double1 c)


clampd4d1d1 :
    Glsl.Expression Glsl.DVec4
    -> Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.DVec4
clampd4d1d1 a b c =
    Glsl.unsafeCall3 "clamp" [] a b c


clampd4d1wd1 :
    Glsl.Expression Glsl.DVec4
    -> Glsl.Expression Glsl.Double
    -> Float
    -> Glsl.Expression Glsl.DVec4
clampd4d1wd1 a b c =
    Glsl.unsafeCall3 "clamp" [] a b (Glsl.double1 c)


clampd4d4d4 :
    Glsl.Expression Glsl.DVec4
    -> Glsl.Expression Glsl.DVec4
    -> Glsl.Expression Glsl.DVec4
    -> Glsl.Expression Glsl.DVec4
clampd4d4d4 a b c =
    Glsl.unsafeCall3 "clamp" [] a b c


clampd4wd1d1 :
    Glsl.Expression Glsl.DVec4
    -> Float
    -> Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.DVec4
clampd4wd1d1 a b c =
    Glsl.unsafeCall3 "clamp" [] a (Glsl.double1 b) c


clampd4wd1wd1 : Glsl.Expression Glsl.DVec4 -> Float -> Float -> Glsl.Expression Glsl.DVec4
clampd4wd1wd1 a b c =
    Glsl.unsafeCall3 "clamp" [] a (Glsl.double1 b) (Glsl.double1 c)


clampf11 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
clampf11 a b c =
    Glsl.unsafeCall3 "clamp" [] (Glsl.float1 a) b c


clampf1f : Float -> Glsl.Expression Glsl.Float_ -> Float -> Glsl.Expression Glsl.Float_
clampf1f a b c =
    Glsl.unsafeCall3 "clamp" [] (Glsl.float1 a) b (Glsl.float1 c)


clampff1 : Float -> Float -> Glsl.Expression Glsl.Float_ -> Glsl.Expression Glsl.Float_
clampff1 a b c =
    Glsl.unsafeCall3 "clamp" [] (Glsl.float1 a) (Glsl.float1 b) c


clampfff : Float -> Float -> Float -> Glsl.Expression Glsl.Float_
clampfff a b c =
    Glsl.unsafeCall3 "clamp" [] (Glsl.float1 a) (Glsl.float1 b) (Glsl.float1 c)


clampi1i1i1 :
    Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
clampi1i1i1 a b c =
    Glsl.unsafeCall3 "clamp" [] a b c


clampi1i1wi1 :
    Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Int
    -> Glsl.Expression Glsl.Int_
clampi1i1wi1 a b c =
    Glsl.unsafeCall3 "clamp" [] a b (Glsl.int1 c)


clampi1wi1i1 :
    Glsl.Expression Glsl.Int_
    -> Int
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
clampi1wi1i1 a b c =
    Glsl.unsafeCall3 "clamp" [] a (Glsl.int1 b) c


clampi1wi1wi1 : Glsl.Expression Glsl.Int_ -> Int -> Int -> Glsl.Expression Glsl.Int_
clampi1wi1wi1 a b c =
    Glsl.unsafeCall3 "clamp" [] a (Glsl.int1 b) (Glsl.int1 c)


clampi2i1i1 :
    Glsl.Expression Glsl.IVec2
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.IVec2
clampi2i1i1 a b c =
    Glsl.unsafeCall3 "clamp" [] a b c


clampi2i1wi1 :
    Glsl.Expression Glsl.IVec2
    -> Glsl.Expression Glsl.Int_
    -> Int
    -> Glsl.Expression Glsl.IVec2
clampi2i1wi1 a b c =
    Glsl.unsafeCall3 "clamp" [] a b (Glsl.int1 c)


clampi2i2i2 :
    Glsl.Expression Glsl.IVec2
    -> Glsl.Expression Glsl.IVec2
    -> Glsl.Expression Glsl.IVec2
    -> Glsl.Expression Glsl.IVec2
clampi2i2i2 a b c =
    Glsl.unsafeCall3 "clamp" [] a b c


clampi2wi1i1 :
    Glsl.Expression Glsl.IVec2
    -> Int
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.IVec2
clampi2wi1i1 a b c =
    Glsl.unsafeCall3 "clamp" [] a (Glsl.int1 b) c


clampi2wi1wi1 : Glsl.Expression Glsl.IVec2 -> Int -> Int -> Glsl.Expression Glsl.IVec2
clampi2wi1wi1 a b c =
    Glsl.unsafeCall3 "clamp" [] a (Glsl.int1 b) (Glsl.int1 c)


clampi3i1i1 :
    Glsl.Expression Glsl.IVec3
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.IVec3
clampi3i1i1 a b c =
    Glsl.unsafeCall3 "clamp" [] a b c


clampi3i1wi1 :
    Glsl.Expression Glsl.IVec3
    -> Glsl.Expression Glsl.Int_
    -> Int
    -> Glsl.Expression Glsl.IVec3
clampi3i1wi1 a b c =
    Glsl.unsafeCall3 "clamp" [] a b (Glsl.int1 c)


clampi3i3i3 :
    Glsl.Expression Glsl.IVec3
    -> Glsl.Expression Glsl.IVec3
    -> Glsl.Expression Glsl.IVec3
    -> Glsl.Expression Glsl.IVec3
clampi3i3i3 a b c =
    Glsl.unsafeCall3 "clamp" [] a b c


clampi3wi1i1 :
    Glsl.Expression Glsl.IVec3
    -> Int
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.IVec3
clampi3wi1i1 a b c =
    Glsl.unsafeCall3 "clamp" [] a (Glsl.int1 b) c


clampi3wi1wi1 : Glsl.Expression Glsl.IVec3 -> Int -> Int -> Glsl.Expression Glsl.IVec3
clampi3wi1wi1 a b c =
    Glsl.unsafeCall3 "clamp" [] a (Glsl.int1 b) (Glsl.int1 c)


clampi4i1i1 :
    Glsl.Expression Glsl.IVec4
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.IVec4
clampi4i1i1 a b c =
    Glsl.unsafeCall3 "clamp" [] a b c


clampi4i1wi1 :
    Glsl.Expression Glsl.IVec4
    -> Glsl.Expression Glsl.Int_
    -> Int
    -> Glsl.Expression Glsl.IVec4
clampi4i1wi1 a b c =
    Glsl.unsafeCall3 "clamp" [] a b (Glsl.int1 c)


clampi4i4i4 :
    Glsl.Expression Glsl.IVec4
    -> Glsl.Expression Glsl.IVec4
    -> Glsl.Expression Glsl.IVec4
    -> Glsl.Expression Glsl.IVec4
clampi4i4i4 a b c =
    Glsl.unsafeCall3 "clamp" [] a b c


clampi4wi1i1 :
    Glsl.Expression Glsl.IVec4
    -> Int
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.IVec4
clampi4wi1i1 a b c =
    Glsl.unsafeCall3 "clamp" [] a (Glsl.int1 b) c


clampi4wi1wi1 : Glsl.Expression Glsl.IVec4 -> Int -> Int -> Glsl.Expression Glsl.IVec4
clampi4wi1wi1 a b c =
    Glsl.unsafeCall3 "clamp" [] a (Glsl.int1 b) (Glsl.int1 c)


clampu1u1u1 :
    Glsl.Expression Glsl.Uint
    -> Glsl.Expression Glsl.Uint
    -> Glsl.Expression Glsl.Uint
    -> Glsl.Expression Glsl.Uint
clampu1u1u1 a b c =
    Glsl.unsafeCall3 "clamp" [] a b c


clampu1u1wu1 :
    Glsl.Expression Glsl.Uint
    -> Glsl.Expression Glsl.Uint
    -> Int
    -> Glsl.Expression Glsl.Uint
clampu1u1wu1 a b c =
    Glsl.unsafeCall3 "clamp" [] a b (Glsl.uint1 c)


clampu1wu1u1 :
    Glsl.Expression Glsl.Uint
    -> Int
    -> Glsl.Expression Glsl.Uint
    -> Glsl.Expression Glsl.Uint
clampu1wu1u1 a b c =
    Glsl.unsafeCall3 "clamp" [] a (Glsl.uint1 b) c


clampu1wu1wu1 : Glsl.Expression Glsl.Uint -> Int -> Int -> Glsl.Expression Glsl.Uint
clampu1wu1wu1 a b c =
    Glsl.unsafeCall3 "clamp" [] a (Glsl.uint1 b) (Glsl.uint1 c)


clampu2u1u1 :
    Glsl.Expression Glsl.UVec2
    -> Glsl.Expression Glsl.Uint
    -> Glsl.Expression Glsl.Uint
    -> Glsl.Expression Glsl.UVec2
clampu2u1u1 a b c =
    Glsl.unsafeCall3 "clamp" [] a b c


clampu2u1wu1 :
    Glsl.Expression Glsl.UVec2
    -> Glsl.Expression Glsl.Uint
    -> Int
    -> Glsl.Expression Glsl.UVec2
clampu2u1wu1 a b c =
    Glsl.unsafeCall3 "clamp" [] a b (Glsl.uint1 c)


clampu2u2u2 :
    Glsl.Expression Glsl.UVec2
    -> Glsl.Expression Glsl.UVec2
    -> Glsl.Expression Glsl.UVec2
    -> Glsl.Expression Glsl.UVec2
clampu2u2u2 a b c =
    Glsl.unsafeCall3 "clamp" [] a b c


clampu2wu1u1 :
    Glsl.Expression Glsl.UVec2
    -> Int
    -> Glsl.Expression Glsl.Uint
    -> Glsl.Expression Glsl.UVec2
clampu2wu1u1 a b c =
    Glsl.unsafeCall3 "clamp" [] a (Glsl.uint1 b) c


clampu2wu1wu1 : Glsl.Expression Glsl.UVec2 -> Int -> Int -> Glsl.Expression Glsl.UVec2
clampu2wu1wu1 a b c =
    Glsl.unsafeCall3 "clamp" [] a (Glsl.uint1 b) (Glsl.uint1 c)


clampu3u1u1 :
    Glsl.Expression Glsl.UVec3
    -> Glsl.Expression Glsl.Uint
    -> Glsl.Expression Glsl.Uint
    -> Glsl.Expression Glsl.UVec3
clampu3u1u1 a b c =
    Glsl.unsafeCall3 "clamp" [] a b c


clampu3u1wu1 :
    Glsl.Expression Glsl.UVec3
    -> Glsl.Expression Glsl.Uint
    -> Int
    -> Glsl.Expression Glsl.UVec3
clampu3u1wu1 a b c =
    Glsl.unsafeCall3 "clamp" [] a b (Glsl.uint1 c)


clampu3u3u3 :
    Glsl.Expression Glsl.UVec3
    -> Glsl.Expression Glsl.UVec3
    -> Glsl.Expression Glsl.UVec3
    -> Glsl.Expression Glsl.UVec3
clampu3u3u3 a b c =
    Glsl.unsafeCall3 "clamp" [] a b c


clampu3wu1u1 :
    Glsl.Expression Glsl.UVec3
    -> Int
    -> Glsl.Expression Glsl.Uint
    -> Glsl.Expression Glsl.UVec3
clampu3wu1u1 a b c =
    Glsl.unsafeCall3 "clamp" [] a (Glsl.uint1 b) c


clampu3wu1wu1 : Glsl.Expression Glsl.UVec3 -> Int -> Int -> Glsl.Expression Glsl.UVec3
clampu3wu1wu1 a b c =
    Glsl.unsafeCall3 "clamp" [] a (Glsl.uint1 b) (Glsl.uint1 c)


clampu4u1u1 :
    Glsl.Expression Glsl.UVec4
    -> Glsl.Expression Glsl.Uint
    -> Glsl.Expression Glsl.Uint
    -> Glsl.Expression Glsl.UVec4
clampu4u1u1 a b c =
    Glsl.unsafeCall3 "clamp" [] a b c


clampu4u1wu1 :
    Glsl.Expression Glsl.UVec4
    -> Glsl.Expression Glsl.Uint
    -> Int
    -> Glsl.Expression Glsl.UVec4
clampu4u1wu1 a b c =
    Glsl.unsafeCall3 "clamp" [] a b (Glsl.uint1 c)


clampu4u4u4 :
    Glsl.Expression Glsl.UVec4
    -> Glsl.Expression Glsl.UVec4
    -> Glsl.Expression Glsl.UVec4
    -> Glsl.Expression Glsl.UVec4
clampu4u4u4 a b c =
    Glsl.unsafeCall3 "clamp" [] a b c


clampu4wu1u1 :
    Glsl.Expression Glsl.UVec4
    -> Int
    -> Glsl.Expression Glsl.Uint
    -> Glsl.Expression Glsl.UVec4
clampu4wu1u1 a b c =
    Glsl.unsafeCall3 "clamp" [] a (Glsl.uint1 b) c


clampu4wu1wu1 : Glsl.Expression Glsl.UVec4 -> Int -> Int -> Glsl.Expression Glsl.UVec4
clampu4wu1wu1 a b c =
    Glsl.unsafeCall3 "clamp" [] a (Glsl.uint1 b) (Glsl.uint1 c)


clampwd1d1d1 :
    Float
    -> Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.Double
clampwd1d1d1 a b c =
    Glsl.unsafeCall3 "clamp" [] (Glsl.double1 a) b c


clampwd1d1wd1 : Float -> Glsl.Expression Glsl.Double -> Float -> Glsl.Expression Glsl.Double
clampwd1d1wd1 a b c =
    Glsl.unsafeCall3 "clamp" [] (Glsl.double1 a) b (Glsl.double1 c)


clampwd1wd1d1 : Float -> Float -> Glsl.Expression Glsl.Double -> Glsl.Expression Glsl.Double
clampwd1wd1d1 a b c =
    Glsl.unsafeCall3 "clamp" [] (Glsl.double1 a) (Glsl.double1 b) c


clampwd1wd1wd1 : Float -> Float -> Float -> Glsl.Expression Glsl.Double
clampwd1wd1wd1 a b c =
    Glsl.unsafeCall3
        "clamp"
        []
        (Glsl.double1 a)
        (Glsl.double1 b)
        (Glsl.double1 c)


clampwi1i1i1 :
    Int
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
clampwi1i1i1 a b c =
    Glsl.unsafeCall3 "clamp" [] (Glsl.int1 a) b c


clampwi1i1wi1 : Int -> Glsl.Expression Glsl.Int_ -> Int -> Glsl.Expression Glsl.Int_
clampwi1i1wi1 a b c =
    Glsl.unsafeCall3 "clamp" [] (Glsl.int1 a) b (Glsl.int1 c)


clampwi1wi1i1 : Int -> Int -> Glsl.Expression Glsl.Int_ -> Glsl.Expression Glsl.Int_
clampwi1wi1i1 a b c =
    Glsl.unsafeCall3 "clamp" [] (Glsl.int1 a) (Glsl.int1 b) c


clampwi1wi1wi1 : Int -> Int -> Int -> Glsl.Expression Glsl.Int_
clampwi1wi1wi1 a b c =
    Glsl.unsafeCall3 "clamp" [] (Glsl.int1 a) (Glsl.int1 b) (Glsl.int1 c)


clampwu1u1u1 :
    Int
    -> Glsl.Expression Glsl.Uint
    -> Glsl.Expression Glsl.Uint
    -> Glsl.Expression Glsl.Uint
clampwu1u1u1 a b c =
    Glsl.unsafeCall3 "clamp" [] (Glsl.uint1 a) b c


clampwu1u1wu1 : Int -> Glsl.Expression Glsl.Uint -> Int -> Glsl.Expression Glsl.Uint
clampwu1u1wu1 a b c =
    Glsl.unsafeCall3 "clamp" [] (Glsl.uint1 a) b (Glsl.uint1 c)


clampwu1wu1u1 : Int -> Int -> Glsl.Expression Glsl.Uint -> Glsl.Expression Glsl.Uint
clampwu1wu1u1 a b c =
    Glsl.unsafeCall3 "clamp" [] (Glsl.uint1 a) (Glsl.uint1 b) c


clampwu1wu1wu1 : Int -> Int -> Int -> Glsl.Expression Glsl.Uint
clampwu1wu1wu1 a b c =
    Glsl.unsafeCall3 "clamp" [] (Glsl.uint1 a) (Glsl.uint1 b) (Glsl.uint1 c)


cross33 :
    Glsl.Expression Glsl.Vec3
    -> Glsl.Expression Glsl.Vec3
    -> Glsl.Expression Glsl.Vec3
cross33 a b =
    Glsl.unsafeCall2 "cross" [] a b


crossd3d3 :
    Glsl.Expression Glsl.DVec3
    -> Glsl.Expression Glsl.DVec3
    -> Glsl.Expression Glsl.DVec3
crossd3d3 a b =
    Glsl.unsafeCall2 "cross" [] a b


distance11 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
distance11 a b =
    Glsl.unsafeCall2 "distance" [] a b


distance1f : Glsl.Expression Glsl.Float_ -> Float -> Glsl.Expression Glsl.Float_
distance1f a b =
    Glsl.unsafeCall2 "distance" [] a (Glsl.float1 b)


distance22 :
    Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Float_
distance22 a b =
    Glsl.unsafeCall2 "distance" [] a b


distance33 :
    Glsl.Expression Glsl.Vec3
    -> Glsl.Expression Glsl.Vec3
    -> Glsl.Expression Glsl.Float_
distance33 a b =
    Glsl.unsafeCall2 "distance" [] a b


distance44 :
    Glsl.Expression Glsl.Vec4
    -> Glsl.Expression Glsl.Vec4
    -> Glsl.Expression Glsl.Float_
distance44 a b =
    Glsl.unsafeCall2 "distance" [] a b


distanced1d1 :
    Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.Double
distanced1d1 a b =
    Glsl.unsafeCall2 "distance" [] a b


distanced1wd1 : Glsl.Expression Glsl.Double -> Float -> Glsl.Expression Glsl.Double
distanced1wd1 a b =
    Glsl.unsafeCall2 "distance" [] a (Glsl.double1 b)


distanced2d2 :
    Glsl.Expression Glsl.DVec2
    -> Glsl.Expression Glsl.DVec2
    -> Glsl.Expression Glsl.Double
distanced2d2 a b =
    Glsl.unsafeCall2 "distance" [] a b


distanced3d3 :
    Glsl.Expression Glsl.DVec3
    -> Glsl.Expression Glsl.DVec3
    -> Glsl.Expression Glsl.Double
distanced3d3 a b =
    Glsl.unsafeCall2 "distance" [] a b


distanced4d4 :
    Glsl.Expression Glsl.DVec4
    -> Glsl.Expression Glsl.DVec4
    -> Glsl.Expression Glsl.Double
distanced4d4 a b =
    Glsl.unsafeCall2 "distance" [] a b


distancef1 : Float -> Glsl.Expression Glsl.Float_ -> Glsl.Expression Glsl.Float_
distancef1 a b =
    Glsl.unsafeCall2 "distance" [] (Glsl.float1 a) b


distanceff : Float -> Float -> Glsl.Expression Glsl.Float_
distanceff a b =
    Glsl.unsafeCall2 "distance" [] (Glsl.float1 a) (Glsl.float1 b)


distancewd1d1 : Float -> Glsl.Expression Glsl.Double -> Glsl.Expression Glsl.Double
distancewd1d1 a b =
    Glsl.unsafeCall2 "distance" [] (Glsl.double1 a) b


distancewd1wd1 : Float -> Float -> Glsl.Expression Glsl.Double
distancewd1wd1 a b =
    Glsl.unsafeCall2 "distance" [] (Glsl.double1 a) (Glsl.double1 b)


distance :
    Glsl.Expression (Glsl.Vec t a)
    -> Glsl.Expression (Glsl.Vec t a)
    -> Glsl.Expression (Glsl.Vec t Glsl.D1)
distance a b =
    Glsl.unsafeCall2 "distance" [] a b


dot11 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
dot11 a b =
    Glsl.unsafeCall2 "dot" [] a b


dot1f : Glsl.Expression Glsl.Float_ -> Float -> Glsl.Expression Glsl.Float_
dot1f a b =
    Glsl.unsafeCall2 "dot" [] a (Glsl.float1 b)


dot22 :
    Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Float_
dot22 a b =
    Glsl.unsafeCall2 "dot" [] a b


dot33 :
    Glsl.Expression Glsl.Vec3
    -> Glsl.Expression Glsl.Vec3
    -> Glsl.Expression Glsl.Float_
dot33 a b =
    Glsl.unsafeCall2 "dot" [] a b


dot44 :
    Glsl.Expression Glsl.Vec4
    -> Glsl.Expression Glsl.Vec4
    -> Glsl.Expression Glsl.Float_
dot44 a b =
    Glsl.unsafeCall2 "dot" [] a b


dotd1d1 :
    Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.Double
dotd1d1 a b =
    Glsl.unsafeCall2 "dot" [] a b


dotd1wd1 : Glsl.Expression Glsl.Double -> Float -> Glsl.Expression Glsl.Double
dotd1wd1 a b =
    Glsl.unsafeCall2 "dot" [] a (Glsl.double1 b)


dotd2d2 :
    Glsl.Expression Glsl.DVec2
    -> Glsl.Expression Glsl.DVec2
    -> Glsl.Expression Glsl.Double
dotd2d2 a b =
    Glsl.unsafeCall2 "dot" [] a b


dotd3d3 :
    Glsl.Expression Glsl.DVec3
    -> Glsl.Expression Glsl.DVec3
    -> Glsl.Expression Glsl.Double
dotd3d3 a b =
    Glsl.unsafeCall2 "dot" [] a b


dotd4d4 :
    Glsl.Expression Glsl.DVec4
    -> Glsl.Expression Glsl.DVec4
    -> Glsl.Expression Glsl.Double
dotd4d4 a b =
    Glsl.unsafeCall2 "dot" [] a b


dotf1 : Float -> Glsl.Expression Glsl.Float_ -> Glsl.Expression Glsl.Float_
dotf1 a b =
    Glsl.unsafeCall2 "dot" [] (Glsl.float1 a) b


dotff : Float -> Float -> Glsl.Expression Glsl.Float_
dotff a b =
    Glsl.unsafeCall2 "dot" [] (Glsl.float1 a) (Glsl.float1 b)


dotwd1d1 : Float -> Glsl.Expression Glsl.Double -> Glsl.Expression Glsl.Double
dotwd1d1 a b =
    Glsl.unsafeCall2 "dot" [] (Glsl.double1 a) b


dotwd1wd1 : Float -> Float -> Glsl.Expression Glsl.Double
dotwd1wd1 a b =
    Glsl.unsafeCall2 "dot" [] (Glsl.double1 a) (Glsl.double1 b)


dot :
    Glsl.Expression (Glsl.Vec t a)
    -> Glsl.Expression (Glsl.Vec t a)
    -> Glsl.Expression (Glsl.Vec t Glsl.D1)
dot a b =
    Glsl.unsafeCall2 "dot" [] a b


faceforward111 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
faceforward111 a b c =
    Glsl.unsafeCall3 "faceforward" [] a b c


faceforward11f :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
faceforward11f a b c =
    Glsl.unsafeCall3 "faceforward" [] a b (Glsl.float1 c)


faceforward1f1 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
faceforward1f1 a b c =
    Glsl.unsafeCall3 "faceforward" [] a (Glsl.float1 b) c


faceforward1ff : Glsl.Expression Glsl.Float_ -> Float -> Float -> Glsl.Expression Glsl.Float_
faceforward1ff a b c =
    Glsl.unsafeCall3 "faceforward" [] a (Glsl.float1 b) (Glsl.float1 c)


faceforward222 :
    Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
faceforward222 a b c =
    Glsl.unsafeCall3 "faceforward" [] a b c


faceforward333 :
    Glsl.Expression Glsl.Vec3
    -> Glsl.Expression Glsl.Vec3
    -> Glsl.Expression Glsl.Vec3
    -> Glsl.Expression Glsl.Vec3
faceforward333 a b c =
    Glsl.unsafeCall3 "faceforward" [] a b c


faceforward444 :
    Glsl.Expression Glsl.Vec4
    -> Glsl.Expression Glsl.Vec4
    -> Glsl.Expression Glsl.Vec4
    -> Glsl.Expression Glsl.Vec4
faceforward444 a b c =
    Glsl.unsafeCall3 "faceforward" [] a b c


faceforwardd1d1d1 :
    Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.Double
faceforwardd1d1d1 a b c =
    Glsl.unsafeCall3 "faceforward" [] a b c


faceforwardd1d1wd1 :
    Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.Double
    -> Float
    -> Glsl.Expression Glsl.Double
faceforwardd1d1wd1 a b c =
    Glsl.unsafeCall3 "faceforward" [] a b (Glsl.double1 c)


faceforwardd1wd1d1 :
    Glsl.Expression Glsl.Double
    -> Float
    -> Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.Double
faceforwardd1wd1d1 a b c =
    Glsl.unsafeCall3 "faceforward" [] a (Glsl.double1 b) c


faceforwardd1wd1wd1 : Glsl.Expression Glsl.Double -> Float -> Float -> Glsl.Expression Glsl.Double
faceforwardd1wd1wd1 a b c =
    Glsl.unsafeCall3 "faceforward" [] a (Glsl.double1 b) (Glsl.double1 c)


faceforwardd2d2d2 :
    Glsl.Expression Glsl.DVec2
    -> Glsl.Expression Glsl.DVec2
    -> Glsl.Expression Glsl.DVec2
    -> Glsl.Expression Glsl.DVec2
faceforwardd2d2d2 a b c =
    Glsl.unsafeCall3 "faceforward" [] a b c


faceforwardd3d3d3 :
    Glsl.Expression Glsl.DVec3
    -> Glsl.Expression Glsl.DVec3
    -> Glsl.Expression Glsl.DVec3
    -> Glsl.Expression Glsl.DVec3
faceforwardd3d3d3 a b c =
    Glsl.unsafeCall3 "faceforward" [] a b c


faceforwardd4d4d4 :
    Glsl.Expression Glsl.DVec4
    -> Glsl.Expression Glsl.DVec4
    -> Glsl.Expression Glsl.DVec4
    -> Glsl.Expression Glsl.DVec4
faceforwardd4d4d4 a b c =
    Glsl.unsafeCall3 "faceforward" [] a b c


faceforwardf11 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
faceforwardf11 a b c =
    Glsl.unsafeCall3 "faceforward" [] (Glsl.float1 a) b c


faceforwardf1f : Float -> Glsl.Expression Glsl.Float_ -> Float -> Glsl.Expression Glsl.Float_
faceforwardf1f a b c =
    Glsl.unsafeCall3 "faceforward" [] (Glsl.float1 a) b (Glsl.float1 c)


faceforwardff1 : Float -> Float -> Glsl.Expression Glsl.Float_ -> Glsl.Expression Glsl.Float_
faceforwardff1 a b c =
    Glsl.unsafeCall3 "faceforward" [] (Glsl.float1 a) (Glsl.float1 b) c


faceforwardfff : Float -> Float -> Float -> Glsl.Expression Glsl.Float_
faceforwardfff a b c =
    Glsl.unsafeCall3
        "faceforward"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        (Glsl.float1 c)


faceforwardwd1d1d1 :
    Float
    -> Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.Double
faceforwardwd1d1d1 a b c =
    Glsl.unsafeCall3 "faceforward" [] (Glsl.double1 a) b c


faceforwardwd1d1wd1 : Float -> Glsl.Expression Glsl.Double -> Float -> Glsl.Expression Glsl.Double
faceforwardwd1d1wd1 a b c =
    Glsl.unsafeCall3 "faceforward" [] (Glsl.double1 a) b (Glsl.double1 c)


faceforwardwd1wd1d1 : Float -> Float -> Glsl.Expression Glsl.Double -> Glsl.Expression Glsl.Double
faceforwardwd1wd1d1 a b c =
    Glsl.unsafeCall3 "faceforward" [] (Glsl.double1 a) (Glsl.double1 b) c


faceforwardwd1wd1wd1 : Float -> Float -> Float -> Glsl.Expression Glsl.Double
faceforwardwd1wd1wd1 a b c =
    Glsl.unsafeCall3
        "faceforward"
        []
        (Glsl.double1 a)
        (Glsl.double1 b)
        (Glsl.double1 c)


faceforward :
    Glsl.Expression (Glsl.Vec t a)
    -> Glsl.Expression (Glsl.Vec t a)
    -> Glsl.Expression (Glsl.Vec t a)
    -> Glsl.Expression (Glsl.Vec t a)
faceforward a b c =
    Glsl.unsafeCall3 "faceforward" [] a b c


floatBitsToInt1 : Glsl.Expression Glsl.Float_ -> Glsl.Expression Glsl.Int_
floatBitsToInt1 a =
    Glsl.unsafeCall1 "floatBitsToInt" [] a


floatBitsToInt2 : Glsl.Expression Glsl.Vec2 -> Glsl.Expression Glsl.IVec2
floatBitsToInt2 a =
    Glsl.unsafeCall1 "floatBitsToInt" [] a


floatBitsToInt3 : Glsl.Expression Glsl.Vec3 -> Glsl.Expression Glsl.IVec3
floatBitsToInt3 a =
    Glsl.unsafeCall1 "floatBitsToInt" [] a


floatBitsToInt4 : Glsl.Expression Glsl.Vec4 -> Glsl.Expression Glsl.IVec4
floatBitsToInt4 a =
    Glsl.unsafeCall1 "floatBitsToInt" [] a


floatBitsToIntf : Float -> Glsl.Expression Glsl.Int_
floatBitsToIntf a =
    Glsl.unsafeCall1 "floatBitsToInt" [] (Glsl.float1 a)


floatBitsToUint1 : Glsl.Expression Glsl.Float_ -> Glsl.Expression Glsl.Uint
floatBitsToUint1 a =
    Glsl.unsafeCall1 "floatBitsToUint" [] a


floatBitsToUint2 : Glsl.Expression Glsl.Vec2 -> Glsl.Expression Glsl.UVec2
floatBitsToUint2 a =
    Glsl.unsafeCall1 "floatBitsToUint" [] a


floatBitsToUint3 : Glsl.Expression Glsl.Vec3 -> Glsl.Expression Glsl.UVec3
floatBitsToUint3 a =
    Glsl.unsafeCall1 "floatBitsToUint" [] a


floatBitsToUint4 : Glsl.Expression Glsl.Vec4 -> Glsl.Expression Glsl.UVec4
floatBitsToUint4 a =
    Glsl.unsafeCall1 "floatBitsToUint" [] a


floatBitsToUintf : Float -> Glsl.Expression Glsl.Uint
floatBitsToUintf a =
    Glsl.unsafeCall1 "floatBitsToUint" [] (Glsl.float1 a)


floati1 : Glsl.Expression Glsl.Int_ -> Glsl.Expression Glsl.Float_
floati1 a =
    Glsl.unsafeCall1 "float" [] a


floatwi1 : Int -> Glsl.Expression Glsl.Float_
floatwi1 a =
    Glsl.unsafeCall1 "float" [] (Glsl.int1 a)


floor1 : Glsl.Expression Glsl.Float_ -> Glsl.Expression Glsl.Float_
floor1 a =
    Glsl.unsafeCall1 "floor" [] a


floor2 : Glsl.Expression Glsl.Vec2 -> Glsl.Expression Glsl.Vec2
floor2 a =
    Glsl.unsafeCall1 "floor" [] a


floor3 : Glsl.Expression Glsl.Vec3 -> Glsl.Expression Glsl.Vec3
floor3 a =
    Glsl.unsafeCall1 "floor" [] a


floor4 : Glsl.Expression Glsl.Vec4 -> Glsl.Expression Glsl.Vec4
floor4 a =
    Glsl.unsafeCall1 "floor" [] a


floord1 : Glsl.Expression Glsl.Double -> Glsl.Expression Glsl.Double
floord1 a =
    Glsl.unsafeCall1 "floor" [] a


floord2 : Glsl.Expression Glsl.DVec2 -> Glsl.Expression Glsl.DVec2
floord2 a =
    Glsl.unsafeCall1 "floor" [] a


floord3 : Glsl.Expression Glsl.DVec3 -> Glsl.Expression Glsl.DVec3
floord3 a =
    Glsl.unsafeCall1 "floor" [] a


floord4 : Glsl.Expression Glsl.DVec4 -> Glsl.Expression Glsl.DVec4
floord4 a =
    Glsl.unsafeCall1 "floor" [] a


floorf : Float -> Glsl.Expression Glsl.Float_
floorf a =
    Glsl.unsafeCall1 "floor" [] (Glsl.float1 a)


floorwd1 : Float -> Glsl.Expression Glsl.Double
floorwd1 a =
    Glsl.unsafeCall1 "floor" [] (Glsl.double1 a)


fma111 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
fma111 a b c =
    Glsl.unsafeCall3 "fma" [] a b c


fma11f :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
fma11f a b c =
    Glsl.unsafeCall3 "fma" [] a b (Glsl.float1 c)


fma1f1 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
fma1f1 a b c =
    Glsl.unsafeCall3 "fma" [] a (Glsl.float1 b) c


fma1ff : Glsl.Expression Glsl.Float_ -> Float -> Float -> Glsl.Expression Glsl.Float_
fma1ff a b c =
    Glsl.unsafeCall3 "fma" [] a (Glsl.float1 b) (Glsl.float1 c)


fma222 :
    Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
fma222 a b c =
    Glsl.unsafeCall3 "fma" [] a b c


fma333 :
    Glsl.Expression Glsl.Vec3
    -> Glsl.Expression Glsl.Vec3
    -> Glsl.Expression Glsl.Vec3
    -> Glsl.Expression Glsl.Vec3
fma333 a b c =
    Glsl.unsafeCall3 "fma" [] a b c


fma444 :
    Glsl.Expression Glsl.Vec4
    -> Glsl.Expression Glsl.Vec4
    -> Glsl.Expression Glsl.Vec4
    -> Glsl.Expression Glsl.Vec4
fma444 a b c =
    Glsl.unsafeCall3 "fma" [] a b c


fmad1d1d1 :
    Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.Double
fmad1d1d1 a b c =
    Glsl.unsafeCall3 "fma" [] a b c


fmad1d1wd1 :
    Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.Double
    -> Float
    -> Glsl.Expression Glsl.Double
fmad1d1wd1 a b c =
    Glsl.unsafeCall3 "fma" [] a b (Glsl.double1 c)


fmad1wd1d1 :
    Glsl.Expression Glsl.Double
    -> Float
    -> Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.Double
fmad1wd1d1 a b c =
    Glsl.unsafeCall3 "fma" [] a (Glsl.double1 b) c


fmad1wd1wd1 : Glsl.Expression Glsl.Double -> Float -> Float -> Glsl.Expression Glsl.Double
fmad1wd1wd1 a b c =
    Glsl.unsafeCall3 "fma" [] a (Glsl.double1 b) (Glsl.double1 c)


fmad2d2d2 :
    Glsl.Expression Glsl.DVec2
    -> Glsl.Expression Glsl.DVec2
    -> Glsl.Expression Glsl.DVec2
    -> Glsl.Expression Glsl.DVec2
fmad2d2d2 a b c =
    Glsl.unsafeCall3 "fma" [] a b c


fmad3d3d3 :
    Glsl.Expression Glsl.DVec3
    -> Glsl.Expression Glsl.DVec3
    -> Glsl.Expression Glsl.DVec3
    -> Glsl.Expression Glsl.DVec3
fmad3d3d3 a b c =
    Glsl.unsafeCall3 "fma" [] a b c


fmad4d4d4 :
    Glsl.Expression Glsl.DVec4
    -> Glsl.Expression Glsl.DVec4
    -> Glsl.Expression Glsl.DVec4
    -> Glsl.Expression Glsl.DVec4
fmad4d4d4 a b c =
    Glsl.unsafeCall3 "fma" [] a b c


fmaf11 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
fmaf11 a b c =
    Glsl.unsafeCall3 "fma" [] (Glsl.float1 a) b c


fmaf1f : Float -> Glsl.Expression Glsl.Float_ -> Float -> Glsl.Expression Glsl.Float_
fmaf1f a b c =
    Glsl.unsafeCall3 "fma" [] (Glsl.float1 a) b (Glsl.float1 c)


fmaff1 : Float -> Float -> Glsl.Expression Glsl.Float_ -> Glsl.Expression Glsl.Float_
fmaff1 a b c =
    Glsl.unsafeCall3 "fma" [] (Glsl.float1 a) (Glsl.float1 b) c


fmafff : Float -> Float -> Float -> Glsl.Expression Glsl.Float_
fmafff a b c =
    Glsl.unsafeCall3 "fma" [] (Glsl.float1 a) (Glsl.float1 b) (Glsl.float1 c)


fmawd1d1d1 :
    Float
    -> Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.Double
fmawd1d1d1 a b c =
    Glsl.unsafeCall3 "fma" [] (Glsl.double1 a) b c


fmawd1d1wd1 : Float -> Glsl.Expression Glsl.Double -> Float -> Glsl.Expression Glsl.Double
fmawd1d1wd1 a b c =
    Glsl.unsafeCall3 "fma" [] (Glsl.double1 a) b (Glsl.double1 c)


fmawd1wd1d1 : Float -> Float -> Glsl.Expression Glsl.Double -> Glsl.Expression Glsl.Double
fmawd1wd1d1 a b c =
    Glsl.unsafeCall3 "fma" [] (Glsl.double1 a) (Glsl.double1 b) c


fmawd1wd1wd1 : Float -> Float -> Float -> Glsl.Expression Glsl.Double
fmawd1wd1wd1 a b c =
    Glsl.unsafeCall3 "fma" [] (Glsl.double1 a) (Glsl.double1 b) (Glsl.double1 c)


fma :
    Glsl.Expression (Glsl.Vec t a)
    -> Glsl.Expression (Glsl.Vec t a)
    -> Glsl.Expression (Glsl.Vec t a)
    -> Glsl.Expression (Glsl.Vec t a)
fma a b c =
    Glsl.unsafeCall3 "fma" [] a b c


fract1 : Glsl.Expression Glsl.Float_ -> Glsl.Expression Glsl.Float_
fract1 a =
    Glsl.unsafeCall1 "fract" [] a


fract2 : Glsl.Expression Glsl.Vec2 -> Glsl.Expression Glsl.Vec2
fract2 a =
    Glsl.unsafeCall1 "fract" [] a


fract3 : Glsl.Expression Glsl.Vec3 -> Glsl.Expression Glsl.Vec3
fract3 a =
    Glsl.unsafeCall1 "fract" [] a


fract4 : Glsl.Expression Glsl.Vec4 -> Glsl.Expression Glsl.Vec4
fract4 a =
    Glsl.unsafeCall1 "fract" [] a


fractd1 : Glsl.Expression Glsl.Double -> Glsl.Expression Glsl.Double
fractd1 a =
    Glsl.unsafeCall1 "fract" [] a


fractd2 : Glsl.Expression Glsl.DVec2 -> Glsl.Expression Glsl.DVec2
fractd2 a =
    Glsl.unsafeCall1 "fract" [] a


fractd3 : Glsl.Expression Glsl.DVec3 -> Glsl.Expression Glsl.DVec3
fractd3 a =
    Glsl.unsafeCall1 "fract" [] a


fractd4 : Glsl.Expression Glsl.DVec4 -> Glsl.Expression Glsl.DVec4
fractd4 a =
    Glsl.unsafeCall1 "fract" [] a


fractf : Float -> Glsl.Expression Glsl.Float_
fractf a =
    Glsl.unsafeCall1 "fract" [] (Glsl.float1 a)


fractwd1 : Float -> Glsl.Expression Glsl.Double
fractwd1 a =
    Glsl.unsafeCall1 "fract" [] (Glsl.double1 a)


fract : Glsl.Expression (Glsl.Vec t a) -> Glsl.Expression (Glsl.Vec t a)
fract a =
    Glsl.unsafeCall1 "fract" [] a


frexp1oi1 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression (Glsl.Out Glsl.Int_)
    -> Glsl.Expression Glsl.Float_
frexp1oi1 a b =
    Glsl.unsafeCall2 "frexp" [] a b


frexp1owi1 : Glsl.Expression Glsl.Float_ -> Int -> Glsl.Expression Glsl.Float_
frexp1owi1 a b =
    Glsl.unsafeCall2 "frexp" [] a (Glsl.int1 b)


frexp2oi2 :
    Glsl.Expression Glsl.Vec2
    -> Glsl.Expression (Glsl.Out Glsl.IVec2)
    -> Glsl.Expression Glsl.Vec2
frexp2oi2 a b =
    Glsl.unsafeCall2 "frexp" [] a b


frexp3oi3 :
    Glsl.Expression Glsl.Vec3
    -> Glsl.Expression (Glsl.Out Glsl.IVec3)
    -> Glsl.Expression Glsl.Vec3
frexp3oi3 a b =
    Glsl.unsafeCall2 "frexp" [] a b


frexp4oi4 :
    Glsl.Expression Glsl.Vec4
    -> Glsl.Expression (Glsl.Out Glsl.IVec4)
    -> Glsl.Expression Glsl.Vec4
frexp4oi4 a b =
    Glsl.unsafeCall2 "frexp" [] a b


frexpd1oi1 :
    Glsl.Expression Glsl.Double
    -> Glsl.Expression (Glsl.Out Glsl.Int_)
    -> Glsl.Expression Glsl.Double
frexpd1oi1 a b =
    Glsl.unsafeCall2 "frexp" [] a b


frexpd1owi1 : Glsl.Expression Glsl.Double -> Int -> Glsl.Expression Glsl.Double
frexpd1owi1 a b =
    Glsl.unsafeCall2 "frexp" [] a (Glsl.int1 b)


frexpd2oi2 :
    Glsl.Expression Glsl.DVec2
    -> Glsl.Expression (Glsl.Out Glsl.IVec2)
    -> Glsl.Expression Glsl.DVec2
frexpd2oi2 a b =
    Glsl.unsafeCall2 "frexp" [] a b


frexpd3oi3 :
    Glsl.Expression Glsl.DVec3
    -> Glsl.Expression (Glsl.Out Glsl.IVec3)
    -> Glsl.Expression Glsl.DVec3
frexpd3oi3 a b =
    Glsl.unsafeCall2 "frexp" [] a b


frexpd4oi4 :
    Glsl.Expression Glsl.DVec4
    -> Glsl.Expression (Glsl.Out Glsl.IVec4)
    -> Glsl.Expression Glsl.DVec4
frexpd4oi4 a b =
    Glsl.unsafeCall2 "frexp" [] a b


frexpfoi1 : Float -> Glsl.Expression (Glsl.Out Glsl.Int_) -> Glsl.Expression Glsl.Float_
frexpfoi1 a b =
    Glsl.unsafeCall2 "frexp" [] (Glsl.float1 a) b


frexpfowi1 : Float -> Int -> Glsl.Expression Glsl.Float_
frexpfowi1 a b =
    Glsl.unsafeCall2 "frexp" [] (Glsl.float1 a) (Glsl.int1 b)


frexpwd1oi1 : Float -> Glsl.Expression (Glsl.Out Glsl.Int_) -> Glsl.Expression Glsl.Double
frexpwd1oi1 a b =
    Glsl.unsafeCall2 "frexp" [] (Glsl.double1 a) b


frexpwd1owi1 : Float -> Int -> Glsl.Expression Glsl.Double
frexpwd1owi1 a b =
    Glsl.unsafeCall2 "frexp" [] (Glsl.double1 a) (Glsl.int1 b)


int1 : Glsl.Expression Glsl.Float_ -> Glsl.Expression Glsl.Int_
int1 a =
    Glsl.unsafeCall1 "int" [] a


intf : Float -> Glsl.Expression Glsl.Int_
intf a =
    Glsl.unsafeCall1 "int" [] (Glsl.float1 a)


intBitsToFloati1 : Glsl.Expression Glsl.Int_ -> Glsl.Expression Glsl.Float_
intBitsToFloati1 a =
    Glsl.unsafeCall1 "intBitsToFloat" [] a


intBitsToFloati2 : Glsl.Expression Glsl.IVec2 -> Glsl.Expression Glsl.Vec2
intBitsToFloati2 a =
    Glsl.unsafeCall1 "intBitsToFloat" [] a


intBitsToFloati3 : Glsl.Expression Glsl.IVec3 -> Glsl.Expression Glsl.Vec3
intBitsToFloati3 a =
    Glsl.unsafeCall1 "intBitsToFloat" [] a


intBitsToFloati4 : Glsl.Expression Glsl.IVec4 -> Glsl.Expression Glsl.Vec4
intBitsToFloati4 a =
    Glsl.unsafeCall1 "intBitsToFloat" [] a


intBitsToFloatwi1 : Int -> Glsl.Expression Glsl.Float_
intBitsToFloatwi1 a =
    Glsl.unsafeCall1 "intBitsToFloat" [] (Glsl.int1 a)


inversesqrt1 : Glsl.Expression Glsl.Float_ -> Glsl.Expression Glsl.Float_
inversesqrt1 a =
    Glsl.unsafeCall1 "inversesqrt" [] a


inversesqrt2 : Glsl.Expression Glsl.Vec2 -> Glsl.Expression Glsl.Vec2
inversesqrt2 a =
    Glsl.unsafeCall1 "inversesqrt" [] a


inversesqrt3 : Glsl.Expression Glsl.Vec3 -> Glsl.Expression Glsl.Vec3
inversesqrt3 a =
    Glsl.unsafeCall1 "inversesqrt" [] a


inversesqrt4 : Glsl.Expression Glsl.Vec4 -> Glsl.Expression Glsl.Vec4
inversesqrt4 a =
    Glsl.unsafeCall1 "inversesqrt" [] a


inversesqrtd1 : Glsl.Expression Glsl.Double -> Glsl.Expression Glsl.Double
inversesqrtd1 a =
    Glsl.unsafeCall1 "inversesqrt" [] a


inversesqrtd2 : Glsl.Expression Glsl.DVec2 -> Glsl.Expression Glsl.DVec2
inversesqrtd2 a =
    Glsl.unsafeCall1 "inversesqrt" [] a


inversesqrtd3 : Glsl.Expression Glsl.DVec3 -> Glsl.Expression Glsl.DVec3
inversesqrtd3 a =
    Glsl.unsafeCall1 "inversesqrt" [] a


inversesqrtd4 : Glsl.Expression Glsl.DVec4 -> Glsl.Expression Glsl.DVec4
inversesqrtd4 a =
    Glsl.unsafeCall1 "inversesqrt" [] a


inversesqrtf : Float -> Glsl.Expression Glsl.Float_
inversesqrtf a =
    Glsl.unsafeCall1 "inversesqrt" [] (Glsl.float1 a)


inversesqrtwd1 : Float -> Glsl.Expression Glsl.Double
inversesqrtwd1 a =
    Glsl.unsafeCall1 "inversesqrt" [] (Glsl.double1 a)


inversesqrt : Glsl.Expression (Glsl.Vec t a) -> Glsl.Expression (Glsl.Vec t a)
inversesqrt a =
    Glsl.unsafeCall1 "inversesqrt" [] a


isinf1 : Glsl.Expression Glsl.Float_ -> Glsl.Expression Glsl.Bool_
isinf1 a =
    Glsl.unsafeCall1 "isinf" [] a


isinf2 : Glsl.Expression Glsl.Vec2 -> Glsl.Expression Glsl.BVec2
isinf2 a =
    Glsl.unsafeCall1 "isinf" [] a


isinf3 : Glsl.Expression Glsl.Vec3 -> Glsl.Expression Glsl.BVec3
isinf3 a =
    Glsl.unsafeCall1 "isinf" [] a


isinf4 : Glsl.Expression Glsl.Vec4 -> Glsl.Expression Glsl.BVec4
isinf4 a =
    Glsl.unsafeCall1 "isinf" [] a


isinfd1 : Glsl.Expression Glsl.Double -> Glsl.Expression Glsl.Bool_
isinfd1 a =
    Glsl.unsafeCall1 "isinf" [] a


isinfd2 : Glsl.Expression Glsl.DVec2 -> Glsl.Expression Glsl.BVec2
isinfd2 a =
    Glsl.unsafeCall1 "isinf" [] a


isinfd3 : Glsl.Expression Glsl.DVec3 -> Glsl.Expression Glsl.BVec3
isinfd3 a =
    Glsl.unsafeCall1 "isinf" [] a


isinfd4 : Glsl.Expression Glsl.DVec4 -> Glsl.Expression Glsl.BVec4
isinfd4 a =
    Glsl.unsafeCall1 "isinf" [] a


isinff : Float -> Glsl.Expression Glsl.Bool_
isinff a =
    Glsl.unsafeCall1 "isinf" [] (Glsl.float1 a)


isinfwd1 : Float -> Glsl.Expression Glsl.Bool_
isinfwd1 a =
    Glsl.unsafeCall1 "isinf" [] (Glsl.double1 a)


isnan1 : Glsl.Expression Glsl.Float_ -> Glsl.Expression Glsl.Bool_
isnan1 a =
    Glsl.unsafeCall1 "isnan" [] a


isnan2 : Glsl.Expression Glsl.Vec2 -> Glsl.Expression Glsl.BVec2
isnan2 a =
    Glsl.unsafeCall1 "isnan" [] a


isnan3 : Glsl.Expression Glsl.Vec3 -> Glsl.Expression Glsl.BVec3
isnan3 a =
    Glsl.unsafeCall1 "isnan" [] a


isnan4 : Glsl.Expression Glsl.Vec4 -> Glsl.Expression Glsl.BVec4
isnan4 a =
    Glsl.unsafeCall1 "isnan" [] a


isnand1 : Glsl.Expression Glsl.Double -> Glsl.Expression Glsl.Bool_
isnand1 a =
    Glsl.unsafeCall1 "isnan" [] a


isnand2 : Glsl.Expression Glsl.DVec2 -> Glsl.Expression Glsl.BVec2
isnand2 a =
    Glsl.unsafeCall1 "isnan" [] a


isnand3 : Glsl.Expression Glsl.DVec3 -> Glsl.Expression Glsl.BVec3
isnand3 a =
    Glsl.unsafeCall1 "isnan" [] a


isnand4 : Glsl.Expression Glsl.DVec4 -> Glsl.Expression Glsl.BVec4
isnand4 a =
    Glsl.unsafeCall1 "isnan" [] a


isnanf : Float -> Glsl.Expression Glsl.Bool_
isnanf a =
    Glsl.unsafeCall1 "isnan" [] (Glsl.float1 a)


isnanwd1 : Float -> Glsl.Expression Glsl.Bool_
isnanwd1 a =
    Glsl.unsafeCall1 "isnan" [] (Glsl.double1 a)


ivec2i1i1 :
    Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec2
ivec2i1i1 a b =
    Glsl.unsafeCall2 "ivec2" [] a b


ivec2i1wi1 : Glsl.Expression Glsl.Int_ -> Int -> Glsl.Expression Glsl.Vec2
ivec2i1wi1 a b =
    Glsl.unsafeCall2 "ivec2" [] a (Glsl.int1 b)


ivec2wi1i1 : Int -> Glsl.Expression Glsl.Int_ -> Glsl.Expression Glsl.Vec2
ivec2wi1i1 a b =
    Glsl.unsafeCall2 "ivec2" [] (Glsl.int1 a) b


ivec2wi1wi1 : Int -> Int -> Glsl.Expression Glsl.Vec2
ivec2wi1wi1 a b =
    Glsl.unsafeCall2 "ivec2" [] (Glsl.int1 a) (Glsl.int1 b)


ivec3i1i1i1 :
    Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec3
ivec3i1i1i1 a b c =
    Glsl.unsafeCall3 "ivec3" [] a b c


ivec3i1i1wi1 :
    Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Int
    -> Glsl.Expression Glsl.Vec3
ivec3i1i1wi1 a b c =
    Glsl.unsafeCall3 "ivec3" [] a b (Glsl.int1 c)


ivec3i1wi1i1 :
    Glsl.Expression Glsl.Int_
    -> Int
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec3
ivec3i1wi1i1 a b c =
    Glsl.unsafeCall3 "ivec3" [] a (Glsl.int1 b) c


ivec3i1wi1wi1 : Glsl.Expression Glsl.Int_ -> Int -> Int -> Glsl.Expression Glsl.Vec3
ivec3i1wi1wi1 a b c =
    Glsl.unsafeCall3 "ivec3" [] a (Glsl.int1 b) (Glsl.int1 c)


ivec3wi1i1i1 :
    Int
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec3
ivec3wi1i1i1 a b c =
    Glsl.unsafeCall3 "ivec3" [] (Glsl.int1 a) b c


ivec3wi1i1wi1 : Int -> Glsl.Expression Glsl.Int_ -> Int -> Glsl.Expression Glsl.Vec3
ivec3wi1i1wi1 a b c =
    Glsl.unsafeCall3 "ivec3" [] (Glsl.int1 a) b (Glsl.int1 c)


ivec3wi1wi1i1 : Int -> Int -> Glsl.Expression Glsl.Int_ -> Glsl.Expression Glsl.Vec3
ivec3wi1wi1i1 a b c =
    Glsl.unsafeCall3 "ivec3" [] (Glsl.int1 a) (Glsl.int1 b) c


ivec3wi1wi1wi1 : Int -> Int -> Int -> Glsl.Expression Glsl.Vec3
ivec3wi1wi1wi1 a b c =
    Glsl.unsafeCall3 "ivec3" [] (Glsl.int1 a) (Glsl.int1 b) (Glsl.int1 c)


ivec4i1i1i1i1 :
    Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec4
ivec4i1i1i1i1 a b c d =
    Glsl.unsafeCall4 "ivec4" [] a b c d


ivec4i1i1i1wi1 :
    Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Int
    -> Glsl.Expression Glsl.Vec4
ivec4i1i1i1wi1 a b c d =
    Glsl.unsafeCall4 "ivec4" [] a b c (Glsl.int1 d)


ivec4i1i1wi1i1 :
    Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Int
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec4
ivec4i1i1wi1i1 a b c d =
    Glsl.unsafeCall4 "ivec4" [] a b (Glsl.int1 c) d


ivec4i1i1wi1wi1 :
    Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Int
    -> Int
    -> Glsl.Expression Glsl.Vec4
ivec4i1i1wi1wi1 a b c d =
    Glsl.unsafeCall4 "ivec4" [] a b (Glsl.int1 c) (Glsl.int1 d)


ivec4i1wi1i1i1 :
    Glsl.Expression Glsl.Int_
    -> Int
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec4
ivec4i1wi1i1i1 a b c d =
    Glsl.unsafeCall4 "ivec4" [] a (Glsl.int1 b) c d


ivec4i1wi1i1wi1 :
    Glsl.Expression Glsl.Int_
    -> Int
    -> Glsl.Expression Glsl.Int_
    -> Int
    -> Glsl.Expression Glsl.Vec4
ivec4i1wi1i1wi1 a b c d =
    Glsl.unsafeCall4 "ivec4" [] a (Glsl.int1 b) c (Glsl.int1 d)


ivec4i1wi1wi1i1 :
    Glsl.Expression Glsl.Int_
    -> Int
    -> Int
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec4
ivec4i1wi1wi1i1 a b c d =
    Glsl.unsafeCall4 "ivec4" [] a (Glsl.int1 b) (Glsl.int1 c) d


ivec4i1wi1wi1wi1 : Glsl.Expression Glsl.Int_ -> Int -> Int -> Int -> Glsl.Expression Glsl.Vec4
ivec4i1wi1wi1wi1 a b c d =
    Glsl.unsafeCall4 "ivec4" [] a (Glsl.int1 b) (Glsl.int1 c) (Glsl.int1 d)


ivec4wi1i1i1i1 :
    Int
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec4
ivec4wi1i1i1i1 a b c d =
    Glsl.unsafeCall4 "ivec4" [] (Glsl.int1 a) b c d


ivec4wi1i1i1wi1 :
    Int
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Int
    -> Glsl.Expression Glsl.Vec4
ivec4wi1i1i1wi1 a b c d =
    Glsl.unsafeCall4 "ivec4" [] (Glsl.int1 a) b c (Glsl.int1 d)


ivec4wi1i1wi1i1 :
    Int
    -> Glsl.Expression Glsl.Int_
    -> Int
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec4
ivec4wi1i1wi1i1 a b c d =
    Glsl.unsafeCall4 "ivec4" [] (Glsl.int1 a) b (Glsl.int1 c) d


ivec4wi1i1wi1wi1 : Int -> Glsl.Expression Glsl.Int_ -> Int -> Int -> Glsl.Expression Glsl.Vec4
ivec4wi1i1wi1wi1 a b c d =
    Glsl.unsafeCall4 "ivec4" [] (Glsl.int1 a) b (Glsl.int1 c) (Glsl.int1 d)


ivec4wi1wi1i1i1 :
    Int
    -> Int
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec4
ivec4wi1wi1i1i1 a b c d =
    Glsl.unsafeCall4 "ivec4" [] (Glsl.int1 a) (Glsl.int1 b) c d


ivec4wi1wi1i1wi1 : Int -> Int -> Glsl.Expression Glsl.Int_ -> Int -> Glsl.Expression Glsl.Vec4
ivec4wi1wi1i1wi1 a b c d =
    Glsl.unsafeCall4 "ivec4" [] (Glsl.int1 a) (Glsl.int1 b) c (Glsl.int1 d)


ivec4wi1wi1wi1i1 : Int -> Int -> Int -> Glsl.Expression Glsl.Int_ -> Glsl.Expression Glsl.Vec4
ivec4wi1wi1wi1i1 a b c d =
    Glsl.unsafeCall4 "ivec4" [] (Glsl.int1 a) (Glsl.int1 b) (Glsl.int1 c) d


ivec4wi1wi1wi1wi1 : Int -> Int -> Int -> Int -> Glsl.Expression Glsl.Vec4
ivec4wi1wi1wi1wi1 a b c d =
    Glsl.unsafeCall4
        "ivec4"
        []
        (Glsl.int1 a)
        (Glsl.int1 b)
        (Glsl.int1 c)
        (Glsl.int1 d)


ldexp1i1 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Float_
ldexp1i1 a b =
    Glsl.unsafeCall2 "ldexp" [] a b


ldexp1wi1 : Glsl.Expression Glsl.Float_ -> Int -> Glsl.Expression Glsl.Float_
ldexp1wi1 a b =
    Glsl.unsafeCall2 "ldexp" [] a (Glsl.int1 b)


ldexp2i2 :
    Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.IVec2
    -> Glsl.Expression Glsl.Vec2
ldexp2i2 a b =
    Glsl.unsafeCall2 "ldexp" [] a b


ldexp3i3 :
    Glsl.Expression Glsl.Vec3
    -> Glsl.Expression Glsl.IVec3
    -> Glsl.Expression Glsl.Vec3
ldexp3i3 a b =
    Glsl.unsafeCall2 "ldexp" [] a b


ldexp4i4 :
    Glsl.Expression Glsl.Vec4
    -> Glsl.Expression Glsl.IVec4
    -> Glsl.Expression Glsl.Vec4
ldexp4i4 a b =
    Glsl.unsafeCall2 "ldexp" [] a b


ldexpd1i1 :
    Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Double
ldexpd1i1 a b =
    Glsl.unsafeCall2 "ldexp" [] a b


ldexpd1wi1 : Glsl.Expression Glsl.Double -> Int -> Glsl.Expression Glsl.Double
ldexpd1wi1 a b =
    Glsl.unsafeCall2 "ldexp" [] a (Glsl.int1 b)


ldexpd2i2 :
    Glsl.Expression Glsl.DVec2
    -> Glsl.Expression Glsl.IVec2
    -> Glsl.Expression Glsl.DVec2
ldexpd2i2 a b =
    Glsl.unsafeCall2 "ldexp" [] a b


ldexpd3i3 :
    Glsl.Expression Glsl.DVec3
    -> Glsl.Expression Glsl.IVec3
    -> Glsl.Expression Glsl.DVec3
ldexpd3i3 a b =
    Glsl.unsafeCall2 "ldexp" [] a b


ldexpd4i4 :
    Glsl.Expression Glsl.DVec4
    -> Glsl.Expression Glsl.IVec4
    -> Glsl.Expression Glsl.DVec4
ldexpd4i4 a b =
    Glsl.unsafeCall2 "ldexp" [] a b


ldexpfi1 : Float -> Glsl.Expression Glsl.Int_ -> Glsl.Expression Glsl.Float_
ldexpfi1 a b =
    Glsl.unsafeCall2 "ldexp" [] (Glsl.float1 a) b


ldexpfwi1 : Float -> Int -> Glsl.Expression Glsl.Float_
ldexpfwi1 a b =
    Glsl.unsafeCall2 "ldexp" [] (Glsl.float1 a) (Glsl.int1 b)


ldexpwd1i1 : Float -> Glsl.Expression Glsl.Int_ -> Glsl.Expression Glsl.Double
ldexpwd1i1 a b =
    Glsl.unsafeCall2 "ldexp" [] (Glsl.double1 a) b


ldexpwd1wi1 : Float -> Int -> Glsl.Expression Glsl.Double
ldexpwd1wi1 a b =
    Glsl.unsafeCall2 "ldexp" [] (Glsl.double1 a) (Glsl.int1 b)


length1 : Glsl.Expression Glsl.Float_ -> Glsl.Expression Glsl.Float_
length1 a =
    Glsl.unsafeCall1 "length" [] a


length2 : Glsl.Expression Glsl.Vec2 -> Glsl.Expression Glsl.Float_
length2 a =
    Glsl.unsafeCall1 "length" [] a


length3 : Glsl.Expression Glsl.Vec3 -> Glsl.Expression Glsl.Float_
length3 a =
    Glsl.unsafeCall1 "length" [] a


length4 : Glsl.Expression Glsl.Vec4 -> Glsl.Expression Glsl.Float_
length4 a =
    Glsl.unsafeCall1 "length" [] a


lengthd1 : Glsl.Expression Glsl.Double -> Glsl.Expression Glsl.Double
lengthd1 a =
    Glsl.unsafeCall1 "length" [] a


lengthd2 : Glsl.Expression Glsl.DVec2 -> Glsl.Expression Glsl.Double
lengthd2 a =
    Glsl.unsafeCall1 "length" [] a


lengthd3 : Glsl.Expression Glsl.DVec3 -> Glsl.Expression Glsl.Double
lengthd3 a =
    Glsl.unsafeCall1 "length" [] a


lengthd4 : Glsl.Expression Glsl.DVec4 -> Glsl.Expression Glsl.Double
lengthd4 a =
    Glsl.unsafeCall1 "length" [] a


lengthf : Float -> Glsl.Expression Glsl.Float_
lengthf a =
    Glsl.unsafeCall1 "length" [] (Glsl.float1 a)


lengthwd1 : Float -> Glsl.Expression Glsl.Double
lengthwd1 a =
    Glsl.unsafeCall1 "length" [] (Glsl.double1 a)


length : Glsl.Expression (Glsl.Vec t a) -> Glsl.Expression (Glsl.Vec t Glsl.D1)
length a =
    Glsl.unsafeCall1 "length" [] a


mat21111 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat2
mat21111 a b c d =
    Glsl.unsafeCall4 "mat2" [] a b c d


mat2111f :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat2
mat2111f a b c d =
    Glsl.unsafeCall4 "mat2" [] a b c (Glsl.float1 d)


mat211f1 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat2
mat211f1 a b c d =
    Glsl.unsafeCall4 "mat2" [] a b (Glsl.float1 c) d


mat211ff :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat2
mat211ff a b c d =
    Glsl.unsafeCall4 "mat2" [] a b (Glsl.float1 c) (Glsl.float1 d)


mat21f11 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat2
mat21f11 a b c d =
    Glsl.unsafeCall4 "mat2" [] a (Glsl.float1 b) c d


mat21f1f :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat2
mat21f1f a b c d =
    Glsl.unsafeCall4 "mat2" [] a (Glsl.float1 b) c (Glsl.float1 d)


mat21ff1 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat2
mat21ff1 a b c d =
    Glsl.unsafeCall4 "mat2" [] a (Glsl.float1 b) (Glsl.float1 c) d


mat21fff :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat2
mat21fff a b c d =
    Glsl.unsafeCall4 "mat2" [] a (Glsl.float1 b) (Glsl.float1 c) (Glsl.float1 d)


mat222 :
    Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Mat2
mat222 a b =
    Glsl.unsafeCall2 "mat2" [] a b


mat2f111 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat2
mat2f111 a b c d =
    Glsl.unsafeCall4 "mat2" [] (Glsl.float1 a) b c d


mat2f11f :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat2
mat2f11f a b c d =
    Glsl.unsafeCall4 "mat2" [] (Glsl.float1 a) b c (Glsl.float1 d)


mat2f1f1 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat2
mat2f1f1 a b c d =
    Glsl.unsafeCall4 "mat2" [] (Glsl.float1 a) b (Glsl.float1 c) d


mat2f1ff :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat2
mat2f1ff a b c d =
    Glsl.unsafeCall4 "mat2" [] (Glsl.float1 a) b (Glsl.float1 c) (Glsl.float1 d)


mat2ff11 :
    Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat2
mat2ff11 a b c d =
    Glsl.unsafeCall4 "mat2" [] (Glsl.float1 a) (Glsl.float1 b) c d


mat2ff1f :
    Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat2
mat2ff1f a b c d =
    Glsl.unsafeCall4 "mat2" [] (Glsl.float1 a) (Glsl.float1 b) c (Glsl.float1 d)


mat2fff1 :
    Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat2
mat2fff1 a b c d =
    Glsl.unsafeCall4 "mat2" [] (Glsl.float1 a) (Glsl.float1 b) (Glsl.float1 c) d


mat2ffff : Float -> Float -> Float -> Float -> Glsl.Expression Glsl.Mat2
mat2ffff a b c d =
    Glsl.unsafeCall4
        "mat2"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        (Glsl.float1 c)
        (Glsl.float1 d)


mat3111111111 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3111111111 a b c d e f g h i =
    Glsl.unsafeCall9 "mat3" [] a b c d e f g h i


mat311111111f :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat311111111f a b c d e f g h i =
    Glsl.unsafeCall9 "mat3" [] a b c d e f g h (Glsl.float1 i)


mat31111111f1 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat31111111f1 a b c d e f g h i =
    Glsl.unsafeCall9 "mat3" [] a b c d e f g (Glsl.float1 h) i


mat31111111ff :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat31111111ff a b c d e f g h i =
    Glsl.unsafeCall9 "mat3" [] a b c d e f g (Glsl.float1 h) (Glsl.float1 i)


mat3111111f11 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3111111f11 a b c d e f g h i =
    Glsl.unsafeCall9 "mat3" [] a b c d e f (Glsl.float1 g) h i


mat3111111f1f :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3111111f1f a b c d e f g h i =
    Glsl.unsafeCall9 "mat3" [] a b c d e f (Glsl.float1 g) h (Glsl.float1 i)


mat3111111ff1 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3111111ff1 a b c d e f g h i =
    Glsl.unsafeCall9 "mat3" [] a b c d e f (Glsl.float1 g) (Glsl.float1 h) i


mat3111111fff :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3111111fff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        c
        d
        e
        f
        (Glsl.float1 g)
        (Glsl.float1 h)
        (Glsl.float1 i)


mat311111f111 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat311111f111 a b c d e f g h i =
    Glsl.unsafeCall9 "mat3" [] a b c d e (Glsl.float1 f) g h i


mat311111f11f :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat311111f11f a b c d e f g h i =
    Glsl.unsafeCall9 "mat3" [] a b c d e (Glsl.float1 f) g h (Glsl.float1 i)


mat311111f1f1 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat311111f1f1 a b c d e f g h i =
    Glsl.unsafeCall9 "mat3" [] a b c d e (Glsl.float1 f) g (Glsl.float1 h) i


mat311111f1ff :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat311111f1ff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        c
        d
        e
        (Glsl.float1 f)
        g
        (Glsl.float1 h)
        (Glsl.float1 i)


mat311111ff11 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat311111ff11 a b c d e f g h i =
    Glsl.unsafeCall9 "mat3" [] a b c d e (Glsl.float1 f) (Glsl.float1 g) h i


mat311111ff1f :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat311111ff1f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        c
        d
        e
        (Glsl.float1 f)
        (Glsl.float1 g)
        h
        (Glsl.float1 i)


mat311111fff1 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat311111fff1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        c
        d
        e
        (Glsl.float1 f)
        (Glsl.float1 g)
        (Glsl.float1 h)
        i


mat311111ffff :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat311111ffff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        c
        d
        e
        (Glsl.float1 f)
        (Glsl.float1 g)
        (Glsl.float1 h)
        (Glsl.float1 i)


mat31111f1111 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat31111f1111 a b c d e f g h i =
    Glsl.unsafeCall9 "mat3" [] a b c d (Glsl.float1 e) f g h i


mat31111f111f :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat31111f111f a b c d e f g h i =
    Glsl.unsafeCall9 "mat3" [] a b c d (Glsl.float1 e) f g h (Glsl.float1 i)


mat31111f11f1 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat31111f11f1 a b c d e f g h i =
    Glsl.unsafeCall9 "mat3" [] a b c d (Glsl.float1 e) f g (Glsl.float1 h) i


mat31111f11ff :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat31111f11ff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        c
        d
        (Glsl.float1 e)
        f
        g
        (Glsl.float1 h)
        (Glsl.float1 i)


mat31111f1f11 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat31111f1f11 a b c d e f g h i =
    Glsl.unsafeCall9 "mat3" [] a b c d (Glsl.float1 e) f (Glsl.float1 g) h i


mat31111f1f1f :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat31111f1f1f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        c
        d
        (Glsl.float1 e)
        f
        (Glsl.float1 g)
        h
        (Glsl.float1 i)


mat31111f1ff1 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat31111f1ff1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        c
        d
        (Glsl.float1 e)
        f
        (Glsl.float1 g)
        (Glsl.float1 h)
        i


mat31111f1fff :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat31111f1fff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        c
        d
        (Glsl.float1 e)
        f
        (Glsl.float1 g)
        (Glsl.float1 h)
        (Glsl.float1 i)


mat31111ff111 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat31111ff111 a b c d e f g h i =
    Glsl.unsafeCall9 "mat3" [] a b c d (Glsl.float1 e) (Glsl.float1 f) g h i


mat31111ff11f :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat31111ff11f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        c
        d
        (Glsl.float1 e)
        (Glsl.float1 f)
        g
        h
        (Glsl.float1 i)


mat31111ff1f1 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat31111ff1f1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        c
        d
        (Glsl.float1 e)
        (Glsl.float1 f)
        g
        (Glsl.float1 h)
        i


mat31111ff1ff :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat31111ff1ff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        c
        d
        (Glsl.float1 e)
        (Glsl.float1 f)
        g
        (Glsl.float1 h)
        (Glsl.float1 i)


mat31111fff11 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat31111fff11 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        c
        d
        (Glsl.float1 e)
        (Glsl.float1 f)
        (Glsl.float1 g)
        h
        i


mat31111fff1f :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat31111fff1f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        c
        d
        (Glsl.float1 e)
        (Glsl.float1 f)
        (Glsl.float1 g)
        h
        (Glsl.float1 i)


mat31111ffff1 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat31111ffff1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        c
        d
        (Glsl.float1 e)
        (Glsl.float1 f)
        (Glsl.float1 g)
        (Glsl.float1 h)
        i


mat31111fffff :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat31111fffff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        c
        d
        (Glsl.float1 e)
        (Glsl.float1 f)
        (Glsl.float1 g)
        (Glsl.float1 h)
        (Glsl.float1 i)


mat3111f11111 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3111f11111 a b c d e f g h i =
    Glsl.unsafeCall9 "mat3" [] a b c (Glsl.float1 d) e f g h i


mat3111f1111f :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3111f1111f a b c d e f g h i =
    Glsl.unsafeCall9 "mat3" [] a b c (Glsl.float1 d) e f g h (Glsl.float1 i)


mat3111f111f1 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3111f111f1 a b c d e f g h i =
    Glsl.unsafeCall9 "mat3" [] a b c (Glsl.float1 d) e f g (Glsl.float1 h) i


mat3111f111ff :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3111f111ff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        c
        (Glsl.float1 d)
        e
        f
        g
        (Glsl.float1 h)
        (Glsl.float1 i)


mat3111f11f11 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3111f11f11 a b c d e f g h i =
    Glsl.unsafeCall9 "mat3" [] a b c (Glsl.float1 d) e f (Glsl.float1 g) h i


mat3111f11f1f :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3111f11f1f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        c
        (Glsl.float1 d)
        e
        f
        (Glsl.float1 g)
        h
        (Glsl.float1 i)


mat3111f11ff1 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3111f11ff1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        c
        (Glsl.float1 d)
        e
        f
        (Glsl.float1 g)
        (Glsl.float1 h)
        i


mat3111f11fff :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3111f11fff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        c
        (Glsl.float1 d)
        e
        f
        (Glsl.float1 g)
        (Glsl.float1 h)
        (Glsl.float1 i)


mat3111f1f111 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3111f1f111 a b c d e f g h i =
    Glsl.unsafeCall9 "mat3" [] a b c (Glsl.float1 d) e (Glsl.float1 f) g h i


mat3111f1f11f :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3111f1f11f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        c
        (Glsl.float1 d)
        e
        (Glsl.float1 f)
        g
        h
        (Glsl.float1 i)


mat3111f1f1f1 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3111f1f1f1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        c
        (Glsl.float1 d)
        e
        (Glsl.float1 f)
        g
        (Glsl.float1 h)
        i


mat3111f1f1ff :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3111f1f1ff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        c
        (Glsl.float1 d)
        e
        (Glsl.float1 f)
        g
        (Glsl.float1 h)
        (Glsl.float1 i)


mat3111f1ff11 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3111f1ff11 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        c
        (Glsl.float1 d)
        e
        (Glsl.float1 f)
        (Glsl.float1 g)
        h
        i


mat3111f1ff1f :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3111f1ff1f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        c
        (Glsl.float1 d)
        e
        (Glsl.float1 f)
        (Glsl.float1 g)
        h
        (Glsl.float1 i)


mat3111f1fff1 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3111f1fff1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        c
        (Glsl.float1 d)
        e
        (Glsl.float1 f)
        (Glsl.float1 g)
        (Glsl.float1 h)
        i


mat3111f1ffff :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3111f1ffff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        c
        (Glsl.float1 d)
        e
        (Glsl.float1 f)
        (Glsl.float1 g)
        (Glsl.float1 h)
        (Glsl.float1 i)


mat3111ff1111 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3111ff1111 a b c d e f g h i =
    Glsl.unsafeCall9 "mat3" [] a b c (Glsl.float1 d) (Glsl.float1 e) f g h i


mat3111ff111f :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3111ff111f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        c
        (Glsl.float1 d)
        (Glsl.float1 e)
        f
        g
        h
        (Glsl.float1 i)


mat3111ff11f1 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3111ff11f1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        c
        (Glsl.float1 d)
        (Glsl.float1 e)
        f
        g
        (Glsl.float1 h)
        i


mat3111ff11ff :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3111ff11ff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        c
        (Glsl.float1 d)
        (Glsl.float1 e)
        f
        g
        (Glsl.float1 h)
        (Glsl.float1 i)


mat3111ff1f11 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3111ff1f11 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        c
        (Glsl.float1 d)
        (Glsl.float1 e)
        f
        (Glsl.float1 g)
        h
        i


mat3111ff1f1f :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3111ff1f1f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        c
        (Glsl.float1 d)
        (Glsl.float1 e)
        f
        (Glsl.float1 g)
        h
        (Glsl.float1 i)


mat3111ff1ff1 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3111ff1ff1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        c
        (Glsl.float1 d)
        (Glsl.float1 e)
        f
        (Glsl.float1 g)
        (Glsl.float1 h)
        i


mat3111ff1fff :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3111ff1fff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        c
        (Glsl.float1 d)
        (Glsl.float1 e)
        f
        (Glsl.float1 g)
        (Glsl.float1 h)
        (Glsl.float1 i)


mat3111fff111 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3111fff111 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        c
        (Glsl.float1 d)
        (Glsl.float1 e)
        (Glsl.float1 f)
        g
        h
        i


mat3111fff11f :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3111fff11f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        c
        (Glsl.float1 d)
        (Glsl.float1 e)
        (Glsl.float1 f)
        g
        h
        (Glsl.float1 i)


mat3111fff1f1 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3111fff1f1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        c
        (Glsl.float1 d)
        (Glsl.float1 e)
        (Glsl.float1 f)
        g
        (Glsl.float1 h)
        i


mat3111fff1ff :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3111fff1ff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        c
        (Glsl.float1 d)
        (Glsl.float1 e)
        (Glsl.float1 f)
        g
        (Glsl.float1 h)
        (Glsl.float1 i)


mat3111ffff11 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3111ffff11 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        c
        (Glsl.float1 d)
        (Glsl.float1 e)
        (Glsl.float1 f)
        (Glsl.float1 g)
        h
        i


mat3111ffff1f :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3111ffff1f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        c
        (Glsl.float1 d)
        (Glsl.float1 e)
        (Glsl.float1 f)
        (Glsl.float1 g)
        h
        (Glsl.float1 i)


mat3111fffff1 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3111fffff1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        c
        (Glsl.float1 d)
        (Glsl.float1 e)
        (Glsl.float1 f)
        (Glsl.float1 g)
        (Glsl.float1 h)
        i


mat3111ffffff :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3111ffffff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        c
        (Glsl.float1 d)
        (Glsl.float1 e)
        (Glsl.float1 f)
        (Glsl.float1 g)
        (Glsl.float1 h)
        (Glsl.float1 i)


mat311f111111 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat311f111111 a b c d e f g h i =
    Glsl.unsafeCall9 "mat3" [] a b (Glsl.float1 c) d e f g h i


mat311f11111f :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat311f11111f a b c d e f g h i =
    Glsl.unsafeCall9 "mat3" [] a b (Glsl.float1 c) d e f g h (Glsl.float1 i)


mat311f1111f1 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat311f1111f1 a b c d e f g h i =
    Glsl.unsafeCall9 "mat3" [] a b (Glsl.float1 c) d e f g (Glsl.float1 h) i


mat311f1111ff :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat311f1111ff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        (Glsl.float1 c)
        d
        e
        f
        g
        (Glsl.float1 h)
        (Glsl.float1 i)


mat311f111f11 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat311f111f11 a b c d e f g h i =
    Glsl.unsafeCall9 "mat3" [] a b (Glsl.float1 c) d e f (Glsl.float1 g) h i


mat311f111f1f :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat311f111f1f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        (Glsl.float1 c)
        d
        e
        f
        (Glsl.float1 g)
        h
        (Glsl.float1 i)


mat311f111ff1 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat311f111ff1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        (Glsl.float1 c)
        d
        e
        f
        (Glsl.float1 g)
        (Glsl.float1 h)
        i


mat311f111fff :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat311f111fff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        (Glsl.float1 c)
        d
        e
        f
        (Glsl.float1 g)
        (Glsl.float1 h)
        (Glsl.float1 i)


mat311f11f111 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat311f11f111 a b c d e f g h i =
    Glsl.unsafeCall9 "mat3" [] a b (Glsl.float1 c) d e (Glsl.float1 f) g h i


mat311f11f11f :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat311f11f11f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        (Glsl.float1 c)
        d
        e
        (Glsl.float1 f)
        g
        h
        (Glsl.float1 i)


mat311f11f1f1 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat311f11f1f1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        (Glsl.float1 c)
        d
        e
        (Glsl.float1 f)
        g
        (Glsl.float1 h)
        i


mat311f11f1ff :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat311f11f1ff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        (Glsl.float1 c)
        d
        e
        (Glsl.float1 f)
        g
        (Glsl.float1 h)
        (Glsl.float1 i)


mat311f11ff11 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat311f11ff11 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        (Glsl.float1 c)
        d
        e
        (Glsl.float1 f)
        (Glsl.float1 g)
        h
        i


mat311f11ff1f :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat311f11ff1f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        (Glsl.float1 c)
        d
        e
        (Glsl.float1 f)
        (Glsl.float1 g)
        h
        (Glsl.float1 i)


mat311f11fff1 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat311f11fff1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        (Glsl.float1 c)
        d
        e
        (Glsl.float1 f)
        (Glsl.float1 g)
        (Glsl.float1 h)
        i


mat311f11ffff :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat311f11ffff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        (Glsl.float1 c)
        d
        e
        (Glsl.float1 f)
        (Glsl.float1 g)
        (Glsl.float1 h)
        (Glsl.float1 i)


mat311f1f1111 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat311f1f1111 a b c d e f g h i =
    Glsl.unsafeCall9 "mat3" [] a b (Glsl.float1 c) d (Glsl.float1 e) f g h i


mat311f1f111f :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat311f1f111f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        (Glsl.float1 c)
        d
        (Glsl.float1 e)
        f
        g
        h
        (Glsl.float1 i)


mat311f1f11f1 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat311f1f11f1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        (Glsl.float1 c)
        d
        (Glsl.float1 e)
        f
        g
        (Glsl.float1 h)
        i


mat311f1f11ff :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat311f1f11ff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        (Glsl.float1 c)
        d
        (Glsl.float1 e)
        f
        g
        (Glsl.float1 h)
        (Glsl.float1 i)


mat311f1f1f11 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat311f1f1f11 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        (Glsl.float1 c)
        d
        (Glsl.float1 e)
        f
        (Glsl.float1 g)
        h
        i


mat311f1f1f1f :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat311f1f1f1f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        (Glsl.float1 c)
        d
        (Glsl.float1 e)
        f
        (Glsl.float1 g)
        h
        (Glsl.float1 i)


mat311f1f1ff1 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat311f1f1ff1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        (Glsl.float1 c)
        d
        (Glsl.float1 e)
        f
        (Glsl.float1 g)
        (Glsl.float1 h)
        i


mat311f1f1fff :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat311f1f1fff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        (Glsl.float1 c)
        d
        (Glsl.float1 e)
        f
        (Glsl.float1 g)
        (Glsl.float1 h)
        (Glsl.float1 i)


mat311f1ff111 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat311f1ff111 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        (Glsl.float1 c)
        d
        (Glsl.float1 e)
        (Glsl.float1 f)
        g
        h
        i


mat311f1ff11f :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat311f1ff11f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        (Glsl.float1 c)
        d
        (Glsl.float1 e)
        (Glsl.float1 f)
        g
        h
        (Glsl.float1 i)


mat311f1ff1f1 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat311f1ff1f1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        (Glsl.float1 c)
        d
        (Glsl.float1 e)
        (Glsl.float1 f)
        g
        (Glsl.float1 h)
        i


mat311f1ff1ff :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat311f1ff1ff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        (Glsl.float1 c)
        d
        (Glsl.float1 e)
        (Glsl.float1 f)
        g
        (Glsl.float1 h)
        (Glsl.float1 i)


mat311f1fff11 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat311f1fff11 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        (Glsl.float1 c)
        d
        (Glsl.float1 e)
        (Glsl.float1 f)
        (Glsl.float1 g)
        h
        i


mat311f1fff1f :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat311f1fff1f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        (Glsl.float1 c)
        d
        (Glsl.float1 e)
        (Glsl.float1 f)
        (Glsl.float1 g)
        h
        (Glsl.float1 i)


mat311f1ffff1 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat311f1ffff1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        (Glsl.float1 c)
        d
        (Glsl.float1 e)
        (Glsl.float1 f)
        (Glsl.float1 g)
        (Glsl.float1 h)
        i


mat311f1fffff :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat311f1fffff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        (Glsl.float1 c)
        d
        (Glsl.float1 e)
        (Glsl.float1 f)
        (Glsl.float1 g)
        (Glsl.float1 h)
        (Glsl.float1 i)


mat311ff11111 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat311ff11111 a b c d e f g h i =
    Glsl.unsafeCall9 "mat3" [] a b (Glsl.float1 c) (Glsl.float1 d) e f g h i


mat311ff1111f :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat311ff1111f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        (Glsl.float1 c)
        (Glsl.float1 d)
        e
        f
        g
        h
        (Glsl.float1 i)


mat311ff111f1 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat311ff111f1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        (Glsl.float1 c)
        (Glsl.float1 d)
        e
        f
        g
        (Glsl.float1 h)
        i


mat311ff111ff :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat311ff111ff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        (Glsl.float1 c)
        (Glsl.float1 d)
        e
        f
        g
        (Glsl.float1 h)
        (Glsl.float1 i)


mat311ff11f11 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat311ff11f11 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        (Glsl.float1 c)
        (Glsl.float1 d)
        e
        f
        (Glsl.float1 g)
        h
        i


mat311ff11f1f :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat311ff11f1f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        (Glsl.float1 c)
        (Glsl.float1 d)
        e
        f
        (Glsl.float1 g)
        h
        (Glsl.float1 i)


mat311ff11ff1 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat311ff11ff1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        (Glsl.float1 c)
        (Glsl.float1 d)
        e
        f
        (Glsl.float1 g)
        (Glsl.float1 h)
        i


mat311ff11fff :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat311ff11fff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        (Glsl.float1 c)
        (Glsl.float1 d)
        e
        f
        (Glsl.float1 g)
        (Glsl.float1 h)
        (Glsl.float1 i)


mat311ff1f111 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat311ff1f111 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        (Glsl.float1 c)
        (Glsl.float1 d)
        e
        (Glsl.float1 f)
        g
        h
        i


mat311ff1f11f :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat311ff1f11f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        (Glsl.float1 c)
        (Glsl.float1 d)
        e
        (Glsl.float1 f)
        g
        h
        (Glsl.float1 i)


mat311ff1f1f1 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat311ff1f1f1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        (Glsl.float1 c)
        (Glsl.float1 d)
        e
        (Glsl.float1 f)
        g
        (Glsl.float1 h)
        i


mat311ff1f1ff :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat311ff1f1ff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        (Glsl.float1 c)
        (Glsl.float1 d)
        e
        (Glsl.float1 f)
        g
        (Glsl.float1 h)
        (Glsl.float1 i)


mat311ff1ff11 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat311ff1ff11 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        (Glsl.float1 c)
        (Glsl.float1 d)
        e
        (Glsl.float1 f)
        (Glsl.float1 g)
        h
        i


mat311ff1ff1f :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat311ff1ff1f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        (Glsl.float1 c)
        (Glsl.float1 d)
        e
        (Glsl.float1 f)
        (Glsl.float1 g)
        h
        (Glsl.float1 i)


mat311ff1fff1 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat311ff1fff1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        (Glsl.float1 c)
        (Glsl.float1 d)
        e
        (Glsl.float1 f)
        (Glsl.float1 g)
        (Glsl.float1 h)
        i


mat311ff1ffff :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat311ff1ffff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        (Glsl.float1 c)
        (Glsl.float1 d)
        e
        (Glsl.float1 f)
        (Glsl.float1 g)
        (Glsl.float1 h)
        (Glsl.float1 i)


mat311fff1111 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat311fff1111 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        (Glsl.float1 c)
        (Glsl.float1 d)
        (Glsl.float1 e)
        f
        g
        h
        i


mat311fff111f :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat311fff111f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        (Glsl.float1 c)
        (Glsl.float1 d)
        (Glsl.float1 e)
        f
        g
        h
        (Glsl.float1 i)


mat311fff11f1 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat311fff11f1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        (Glsl.float1 c)
        (Glsl.float1 d)
        (Glsl.float1 e)
        f
        g
        (Glsl.float1 h)
        i


mat311fff11ff :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat311fff11ff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        (Glsl.float1 c)
        (Glsl.float1 d)
        (Glsl.float1 e)
        f
        g
        (Glsl.float1 h)
        (Glsl.float1 i)


mat311fff1f11 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat311fff1f11 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        (Glsl.float1 c)
        (Glsl.float1 d)
        (Glsl.float1 e)
        f
        (Glsl.float1 g)
        h
        i


mat311fff1f1f :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat311fff1f1f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        (Glsl.float1 c)
        (Glsl.float1 d)
        (Glsl.float1 e)
        f
        (Glsl.float1 g)
        h
        (Glsl.float1 i)


mat311fff1ff1 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat311fff1ff1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        (Glsl.float1 c)
        (Glsl.float1 d)
        (Glsl.float1 e)
        f
        (Glsl.float1 g)
        (Glsl.float1 h)
        i


mat311fff1fff :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat311fff1fff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        (Glsl.float1 c)
        (Glsl.float1 d)
        (Glsl.float1 e)
        f
        (Glsl.float1 g)
        (Glsl.float1 h)
        (Glsl.float1 i)


mat311ffff111 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat311ffff111 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        (Glsl.float1 c)
        (Glsl.float1 d)
        (Glsl.float1 e)
        (Glsl.float1 f)
        g
        h
        i


mat311ffff11f :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat311ffff11f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        (Glsl.float1 c)
        (Glsl.float1 d)
        (Glsl.float1 e)
        (Glsl.float1 f)
        g
        h
        (Glsl.float1 i)


mat311ffff1f1 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat311ffff1f1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        (Glsl.float1 c)
        (Glsl.float1 d)
        (Glsl.float1 e)
        (Glsl.float1 f)
        g
        (Glsl.float1 h)
        i


mat311ffff1ff :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat311ffff1ff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        (Glsl.float1 c)
        (Glsl.float1 d)
        (Glsl.float1 e)
        (Glsl.float1 f)
        g
        (Glsl.float1 h)
        (Glsl.float1 i)


mat311fffff11 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat311fffff11 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        (Glsl.float1 c)
        (Glsl.float1 d)
        (Glsl.float1 e)
        (Glsl.float1 f)
        (Glsl.float1 g)
        h
        i


mat311fffff1f :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat311fffff1f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        (Glsl.float1 c)
        (Glsl.float1 d)
        (Glsl.float1 e)
        (Glsl.float1 f)
        (Glsl.float1 g)
        h
        (Glsl.float1 i)


mat311ffffff1 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat311ffffff1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        (Glsl.float1 c)
        (Glsl.float1 d)
        (Glsl.float1 e)
        (Glsl.float1 f)
        (Glsl.float1 g)
        (Glsl.float1 h)
        i


mat311fffffff :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat311fffffff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        b
        (Glsl.float1 c)
        (Glsl.float1 d)
        (Glsl.float1 e)
        (Glsl.float1 f)
        (Glsl.float1 g)
        (Glsl.float1 h)
        (Glsl.float1 i)


mat31f1111111 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat31f1111111 a b c d e f g h i =
    Glsl.unsafeCall9 "mat3" [] a (Glsl.float1 b) c d e f g h i


mat31f111111f :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat31f111111f a b c d e f g h i =
    Glsl.unsafeCall9 "mat3" [] a (Glsl.float1 b) c d e f g h (Glsl.float1 i)


mat31f11111f1 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat31f11111f1 a b c d e f g h i =
    Glsl.unsafeCall9 "mat3" [] a (Glsl.float1 b) c d e f g (Glsl.float1 h) i


mat31f11111ff :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat31f11111ff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        c
        d
        e
        f
        g
        (Glsl.float1 h)
        (Glsl.float1 i)


mat31f1111f11 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat31f1111f11 a b c d e f g h i =
    Glsl.unsafeCall9 "mat3" [] a (Glsl.float1 b) c d e f (Glsl.float1 g) h i


mat31f1111f1f :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat31f1111f1f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        c
        d
        e
        f
        (Glsl.float1 g)
        h
        (Glsl.float1 i)


mat31f1111ff1 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat31f1111ff1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        c
        d
        e
        f
        (Glsl.float1 g)
        (Glsl.float1 h)
        i


mat31f1111fff :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat31f1111fff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        c
        d
        e
        f
        (Glsl.float1 g)
        (Glsl.float1 h)
        (Glsl.float1 i)


mat31f111f111 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat31f111f111 a b c d e f g h i =
    Glsl.unsafeCall9 "mat3" [] a (Glsl.float1 b) c d e (Glsl.float1 f) g h i


mat31f111f11f :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat31f111f11f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        c
        d
        e
        (Glsl.float1 f)
        g
        h
        (Glsl.float1 i)


mat31f111f1f1 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat31f111f1f1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        c
        d
        e
        (Glsl.float1 f)
        g
        (Glsl.float1 h)
        i


mat31f111f1ff :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat31f111f1ff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        c
        d
        e
        (Glsl.float1 f)
        g
        (Glsl.float1 h)
        (Glsl.float1 i)


mat31f111ff11 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat31f111ff11 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        c
        d
        e
        (Glsl.float1 f)
        (Glsl.float1 g)
        h
        i


mat31f111ff1f :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat31f111ff1f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        c
        d
        e
        (Glsl.float1 f)
        (Glsl.float1 g)
        h
        (Glsl.float1 i)


mat31f111fff1 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat31f111fff1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        c
        d
        e
        (Glsl.float1 f)
        (Glsl.float1 g)
        (Glsl.float1 h)
        i


mat31f111ffff :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat31f111ffff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        c
        d
        e
        (Glsl.float1 f)
        (Glsl.float1 g)
        (Glsl.float1 h)
        (Glsl.float1 i)


mat31f11f1111 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat31f11f1111 a b c d e f g h i =
    Glsl.unsafeCall9 "mat3" [] a (Glsl.float1 b) c d (Glsl.float1 e) f g h i


mat31f11f111f :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat31f11f111f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        c
        d
        (Glsl.float1 e)
        f
        g
        h
        (Glsl.float1 i)


mat31f11f11f1 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat31f11f11f1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        c
        d
        (Glsl.float1 e)
        f
        g
        (Glsl.float1 h)
        i


mat31f11f11ff :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat31f11f11ff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        c
        d
        (Glsl.float1 e)
        f
        g
        (Glsl.float1 h)
        (Glsl.float1 i)


mat31f11f1f11 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat31f11f1f11 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        c
        d
        (Glsl.float1 e)
        f
        (Glsl.float1 g)
        h
        i


mat31f11f1f1f :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat31f11f1f1f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        c
        d
        (Glsl.float1 e)
        f
        (Glsl.float1 g)
        h
        (Glsl.float1 i)


mat31f11f1ff1 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat31f11f1ff1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        c
        d
        (Glsl.float1 e)
        f
        (Glsl.float1 g)
        (Glsl.float1 h)
        i


mat31f11f1fff :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat31f11f1fff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        c
        d
        (Glsl.float1 e)
        f
        (Glsl.float1 g)
        (Glsl.float1 h)
        (Glsl.float1 i)


mat31f11ff111 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat31f11ff111 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        c
        d
        (Glsl.float1 e)
        (Glsl.float1 f)
        g
        h
        i


mat31f11ff11f :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat31f11ff11f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        c
        d
        (Glsl.float1 e)
        (Glsl.float1 f)
        g
        h
        (Glsl.float1 i)


mat31f11ff1f1 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat31f11ff1f1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        c
        d
        (Glsl.float1 e)
        (Glsl.float1 f)
        g
        (Glsl.float1 h)
        i


mat31f11ff1ff :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat31f11ff1ff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        c
        d
        (Glsl.float1 e)
        (Glsl.float1 f)
        g
        (Glsl.float1 h)
        (Glsl.float1 i)


mat31f11fff11 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat31f11fff11 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        c
        d
        (Glsl.float1 e)
        (Glsl.float1 f)
        (Glsl.float1 g)
        h
        i


mat31f11fff1f :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat31f11fff1f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        c
        d
        (Glsl.float1 e)
        (Glsl.float1 f)
        (Glsl.float1 g)
        h
        (Glsl.float1 i)


mat31f11ffff1 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat31f11ffff1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        c
        d
        (Glsl.float1 e)
        (Glsl.float1 f)
        (Glsl.float1 g)
        (Glsl.float1 h)
        i


mat31f11fffff :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat31f11fffff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        c
        d
        (Glsl.float1 e)
        (Glsl.float1 f)
        (Glsl.float1 g)
        (Glsl.float1 h)
        (Glsl.float1 i)


mat31f1f11111 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat31f1f11111 a b c d e f g h i =
    Glsl.unsafeCall9 "mat3" [] a (Glsl.float1 b) c (Glsl.float1 d) e f g h i


mat31f1f1111f :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat31f1f1111f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        c
        (Glsl.float1 d)
        e
        f
        g
        h
        (Glsl.float1 i)


mat31f1f111f1 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat31f1f111f1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        c
        (Glsl.float1 d)
        e
        f
        g
        (Glsl.float1 h)
        i


mat31f1f111ff :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat31f1f111ff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        c
        (Glsl.float1 d)
        e
        f
        g
        (Glsl.float1 h)
        (Glsl.float1 i)


mat31f1f11f11 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat31f1f11f11 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        c
        (Glsl.float1 d)
        e
        f
        (Glsl.float1 g)
        h
        i


mat31f1f11f1f :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat31f1f11f1f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        c
        (Glsl.float1 d)
        e
        f
        (Glsl.float1 g)
        h
        (Glsl.float1 i)


mat31f1f11ff1 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat31f1f11ff1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        c
        (Glsl.float1 d)
        e
        f
        (Glsl.float1 g)
        (Glsl.float1 h)
        i


mat31f1f11fff :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat31f1f11fff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        c
        (Glsl.float1 d)
        e
        f
        (Glsl.float1 g)
        (Glsl.float1 h)
        (Glsl.float1 i)


mat31f1f1f111 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat31f1f1f111 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        c
        (Glsl.float1 d)
        e
        (Glsl.float1 f)
        g
        h
        i


mat31f1f1f11f :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat31f1f1f11f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        c
        (Glsl.float1 d)
        e
        (Glsl.float1 f)
        g
        h
        (Glsl.float1 i)


mat31f1f1f1f1 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat31f1f1f1f1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        c
        (Glsl.float1 d)
        e
        (Glsl.float1 f)
        g
        (Glsl.float1 h)
        i


mat31f1f1f1ff :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat31f1f1f1ff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        c
        (Glsl.float1 d)
        e
        (Glsl.float1 f)
        g
        (Glsl.float1 h)
        (Glsl.float1 i)


mat31f1f1ff11 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat31f1f1ff11 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        c
        (Glsl.float1 d)
        e
        (Glsl.float1 f)
        (Glsl.float1 g)
        h
        i


mat31f1f1ff1f :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat31f1f1ff1f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        c
        (Glsl.float1 d)
        e
        (Glsl.float1 f)
        (Glsl.float1 g)
        h
        (Glsl.float1 i)


mat31f1f1fff1 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat31f1f1fff1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        c
        (Glsl.float1 d)
        e
        (Glsl.float1 f)
        (Glsl.float1 g)
        (Glsl.float1 h)
        i


mat31f1f1ffff :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat31f1f1ffff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        c
        (Glsl.float1 d)
        e
        (Glsl.float1 f)
        (Glsl.float1 g)
        (Glsl.float1 h)
        (Glsl.float1 i)


mat31f1ff1111 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat31f1ff1111 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        c
        (Glsl.float1 d)
        (Glsl.float1 e)
        f
        g
        h
        i


mat31f1ff111f :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat31f1ff111f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        c
        (Glsl.float1 d)
        (Glsl.float1 e)
        f
        g
        h
        (Glsl.float1 i)


mat31f1ff11f1 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat31f1ff11f1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        c
        (Glsl.float1 d)
        (Glsl.float1 e)
        f
        g
        (Glsl.float1 h)
        i


mat31f1ff11ff :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat31f1ff11ff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        c
        (Glsl.float1 d)
        (Glsl.float1 e)
        f
        g
        (Glsl.float1 h)
        (Glsl.float1 i)


mat31f1ff1f11 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat31f1ff1f11 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        c
        (Glsl.float1 d)
        (Glsl.float1 e)
        f
        (Glsl.float1 g)
        h
        i


mat31f1ff1f1f :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat31f1ff1f1f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        c
        (Glsl.float1 d)
        (Glsl.float1 e)
        f
        (Glsl.float1 g)
        h
        (Glsl.float1 i)


mat31f1ff1ff1 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat31f1ff1ff1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        c
        (Glsl.float1 d)
        (Glsl.float1 e)
        f
        (Glsl.float1 g)
        (Glsl.float1 h)
        i


mat31f1ff1fff :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat31f1ff1fff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        c
        (Glsl.float1 d)
        (Glsl.float1 e)
        f
        (Glsl.float1 g)
        (Glsl.float1 h)
        (Glsl.float1 i)


mat31f1fff111 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat31f1fff111 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        c
        (Glsl.float1 d)
        (Glsl.float1 e)
        (Glsl.float1 f)
        g
        h
        i


mat31f1fff11f :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat31f1fff11f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        c
        (Glsl.float1 d)
        (Glsl.float1 e)
        (Glsl.float1 f)
        g
        h
        (Glsl.float1 i)


mat31f1fff1f1 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat31f1fff1f1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        c
        (Glsl.float1 d)
        (Glsl.float1 e)
        (Glsl.float1 f)
        g
        (Glsl.float1 h)
        i


mat31f1fff1ff :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat31f1fff1ff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        c
        (Glsl.float1 d)
        (Glsl.float1 e)
        (Glsl.float1 f)
        g
        (Glsl.float1 h)
        (Glsl.float1 i)


mat31f1ffff11 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat31f1ffff11 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        c
        (Glsl.float1 d)
        (Glsl.float1 e)
        (Glsl.float1 f)
        (Glsl.float1 g)
        h
        i


mat31f1ffff1f :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat31f1ffff1f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        c
        (Glsl.float1 d)
        (Glsl.float1 e)
        (Glsl.float1 f)
        (Glsl.float1 g)
        h
        (Glsl.float1 i)


mat31f1fffff1 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat31f1fffff1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        c
        (Glsl.float1 d)
        (Glsl.float1 e)
        (Glsl.float1 f)
        (Glsl.float1 g)
        (Glsl.float1 h)
        i


mat31f1ffffff :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat31f1ffffff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        c
        (Glsl.float1 d)
        (Glsl.float1 e)
        (Glsl.float1 f)
        (Glsl.float1 g)
        (Glsl.float1 h)
        (Glsl.float1 i)


mat31ff111111 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat31ff111111 a b c d e f g h i =
    Glsl.unsafeCall9 "mat3" [] a (Glsl.float1 b) (Glsl.float1 c) d e f g h i


mat31ff11111f :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat31ff11111f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        (Glsl.float1 c)
        d
        e
        f
        g
        h
        (Glsl.float1 i)


mat31ff1111f1 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat31ff1111f1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        (Glsl.float1 c)
        d
        e
        f
        g
        (Glsl.float1 h)
        i


mat31ff1111ff :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat31ff1111ff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        (Glsl.float1 c)
        d
        e
        f
        g
        (Glsl.float1 h)
        (Glsl.float1 i)


mat31ff111f11 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat31ff111f11 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        (Glsl.float1 c)
        d
        e
        f
        (Glsl.float1 g)
        h
        i


mat31ff111f1f :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat31ff111f1f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        (Glsl.float1 c)
        d
        e
        f
        (Glsl.float1 g)
        h
        (Glsl.float1 i)


mat31ff111ff1 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat31ff111ff1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        (Glsl.float1 c)
        d
        e
        f
        (Glsl.float1 g)
        (Glsl.float1 h)
        i


mat31ff111fff :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat31ff111fff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        (Glsl.float1 c)
        d
        e
        f
        (Glsl.float1 g)
        (Glsl.float1 h)
        (Glsl.float1 i)


mat31ff11f111 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat31ff11f111 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        (Glsl.float1 c)
        d
        e
        (Glsl.float1 f)
        g
        h
        i


mat31ff11f11f :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat31ff11f11f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        (Glsl.float1 c)
        d
        e
        (Glsl.float1 f)
        g
        h
        (Glsl.float1 i)


mat31ff11f1f1 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat31ff11f1f1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        (Glsl.float1 c)
        d
        e
        (Glsl.float1 f)
        g
        (Glsl.float1 h)
        i


mat31ff11f1ff :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat31ff11f1ff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        (Glsl.float1 c)
        d
        e
        (Glsl.float1 f)
        g
        (Glsl.float1 h)
        (Glsl.float1 i)


mat31ff11ff11 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat31ff11ff11 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        (Glsl.float1 c)
        d
        e
        (Glsl.float1 f)
        (Glsl.float1 g)
        h
        i


mat31ff11ff1f :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat31ff11ff1f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        (Glsl.float1 c)
        d
        e
        (Glsl.float1 f)
        (Glsl.float1 g)
        h
        (Glsl.float1 i)


mat31ff11fff1 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat31ff11fff1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        (Glsl.float1 c)
        d
        e
        (Glsl.float1 f)
        (Glsl.float1 g)
        (Glsl.float1 h)
        i


mat31ff11ffff :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat31ff11ffff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        (Glsl.float1 c)
        d
        e
        (Glsl.float1 f)
        (Glsl.float1 g)
        (Glsl.float1 h)
        (Glsl.float1 i)


mat31ff1f1111 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat31ff1f1111 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        (Glsl.float1 c)
        d
        (Glsl.float1 e)
        f
        g
        h
        i


mat31ff1f111f :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat31ff1f111f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        (Glsl.float1 c)
        d
        (Glsl.float1 e)
        f
        g
        h
        (Glsl.float1 i)


mat31ff1f11f1 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat31ff1f11f1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        (Glsl.float1 c)
        d
        (Glsl.float1 e)
        f
        g
        (Glsl.float1 h)
        i


mat31ff1f11ff :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat31ff1f11ff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        (Glsl.float1 c)
        d
        (Glsl.float1 e)
        f
        g
        (Glsl.float1 h)
        (Glsl.float1 i)


mat31ff1f1f11 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat31ff1f1f11 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        (Glsl.float1 c)
        d
        (Glsl.float1 e)
        f
        (Glsl.float1 g)
        h
        i


mat31ff1f1f1f :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat31ff1f1f1f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        (Glsl.float1 c)
        d
        (Glsl.float1 e)
        f
        (Glsl.float1 g)
        h
        (Glsl.float1 i)


mat31ff1f1ff1 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat31ff1f1ff1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        (Glsl.float1 c)
        d
        (Glsl.float1 e)
        f
        (Glsl.float1 g)
        (Glsl.float1 h)
        i


mat31ff1f1fff :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat31ff1f1fff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        (Glsl.float1 c)
        d
        (Glsl.float1 e)
        f
        (Glsl.float1 g)
        (Glsl.float1 h)
        (Glsl.float1 i)


mat31ff1ff111 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat31ff1ff111 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        (Glsl.float1 c)
        d
        (Glsl.float1 e)
        (Glsl.float1 f)
        g
        h
        i


mat31ff1ff11f :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat31ff1ff11f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        (Glsl.float1 c)
        d
        (Glsl.float1 e)
        (Glsl.float1 f)
        g
        h
        (Glsl.float1 i)


mat31ff1ff1f1 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat31ff1ff1f1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        (Glsl.float1 c)
        d
        (Glsl.float1 e)
        (Glsl.float1 f)
        g
        (Glsl.float1 h)
        i


mat31ff1ff1ff :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat31ff1ff1ff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        (Glsl.float1 c)
        d
        (Glsl.float1 e)
        (Glsl.float1 f)
        g
        (Glsl.float1 h)
        (Glsl.float1 i)


mat31ff1fff11 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat31ff1fff11 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        (Glsl.float1 c)
        d
        (Glsl.float1 e)
        (Glsl.float1 f)
        (Glsl.float1 g)
        h
        i


mat31ff1fff1f :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat31ff1fff1f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        (Glsl.float1 c)
        d
        (Glsl.float1 e)
        (Glsl.float1 f)
        (Glsl.float1 g)
        h
        (Glsl.float1 i)


mat31ff1ffff1 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat31ff1ffff1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        (Glsl.float1 c)
        d
        (Glsl.float1 e)
        (Glsl.float1 f)
        (Glsl.float1 g)
        (Glsl.float1 h)
        i


mat31ff1fffff :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat31ff1fffff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        (Glsl.float1 c)
        d
        (Glsl.float1 e)
        (Glsl.float1 f)
        (Glsl.float1 g)
        (Glsl.float1 h)
        (Glsl.float1 i)


mat31fff11111 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat31fff11111 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        (Glsl.float1 c)
        (Glsl.float1 d)
        e
        f
        g
        h
        i


mat31fff1111f :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat31fff1111f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        (Glsl.float1 c)
        (Glsl.float1 d)
        e
        f
        g
        h
        (Glsl.float1 i)


mat31fff111f1 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat31fff111f1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        (Glsl.float1 c)
        (Glsl.float1 d)
        e
        f
        g
        (Glsl.float1 h)
        i


mat31fff111ff :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat31fff111ff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        (Glsl.float1 c)
        (Glsl.float1 d)
        e
        f
        g
        (Glsl.float1 h)
        (Glsl.float1 i)


mat31fff11f11 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat31fff11f11 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        (Glsl.float1 c)
        (Glsl.float1 d)
        e
        f
        (Glsl.float1 g)
        h
        i


mat31fff11f1f :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat31fff11f1f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        (Glsl.float1 c)
        (Glsl.float1 d)
        e
        f
        (Glsl.float1 g)
        h
        (Glsl.float1 i)


mat31fff11ff1 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat31fff11ff1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        (Glsl.float1 c)
        (Glsl.float1 d)
        e
        f
        (Glsl.float1 g)
        (Glsl.float1 h)
        i


mat31fff11fff :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat31fff11fff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        (Glsl.float1 c)
        (Glsl.float1 d)
        e
        f
        (Glsl.float1 g)
        (Glsl.float1 h)
        (Glsl.float1 i)


mat31fff1f111 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat31fff1f111 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        (Glsl.float1 c)
        (Glsl.float1 d)
        e
        (Glsl.float1 f)
        g
        h
        i


mat31fff1f11f :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat31fff1f11f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        (Glsl.float1 c)
        (Glsl.float1 d)
        e
        (Glsl.float1 f)
        g
        h
        (Glsl.float1 i)


mat31fff1f1f1 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat31fff1f1f1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        (Glsl.float1 c)
        (Glsl.float1 d)
        e
        (Glsl.float1 f)
        g
        (Glsl.float1 h)
        i


mat31fff1f1ff :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat31fff1f1ff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        (Glsl.float1 c)
        (Glsl.float1 d)
        e
        (Glsl.float1 f)
        g
        (Glsl.float1 h)
        (Glsl.float1 i)


mat31fff1ff11 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat31fff1ff11 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        (Glsl.float1 c)
        (Glsl.float1 d)
        e
        (Glsl.float1 f)
        (Glsl.float1 g)
        h
        i


mat31fff1ff1f :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat31fff1ff1f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        (Glsl.float1 c)
        (Glsl.float1 d)
        e
        (Glsl.float1 f)
        (Glsl.float1 g)
        h
        (Glsl.float1 i)


mat31fff1fff1 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat31fff1fff1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        (Glsl.float1 c)
        (Glsl.float1 d)
        e
        (Glsl.float1 f)
        (Glsl.float1 g)
        (Glsl.float1 h)
        i


mat31fff1ffff :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat31fff1ffff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        (Glsl.float1 c)
        (Glsl.float1 d)
        e
        (Glsl.float1 f)
        (Glsl.float1 g)
        (Glsl.float1 h)
        (Glsl.float1 i)


mat31ffff1111 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat31ffff1111 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        (Glsl.float1 c)
        (Glsl.float1 d)
        (Glsl.float1 e)
        f
        g
        h
        i


mat31ffff111f :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat31ffff111f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        (Glsl.float1 c)
        (Glsl.float1 d)
        (Glsl.float1 e)
        f
        g
        h
        (Glsl.float1 i)


mat31ffff11f1 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat31ffff11f1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        (Glsl.float1 c)
        (Glsl.float1 d)
        (Glsl.float1 e)
        f
        g
        (Glsl.float1 h)
        i


mat31ffff11ff :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat31ffff11ff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        (Glsl.float1 c)
        (Glsl.float1 d)
        (Glsl.float1 e)
        f
        g
        (Glsl.float1 h)
        (Glsl.float1 i)


mat31ffff1f11 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat31ffff1f11 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        (Glsl.float1 c)
        (Glsl.float1 d)
        (Glsl.float1 e)
        f
        (Glsl.float1 g)
        h
        i


mat31ffff1f1f :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat31ffff1f1f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        (Glsl.float1 c)
        (Glsl.float1 d)
        (Glsl.float1 e)
        f
        (Glsl.float1 g)
        h
        (Glsl.float1 i)


mat31ffff1ff1 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat31ffff1ff1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        (Glsl.float1 c)
        (Glsl.float1 d)
        (Glsl.float1 e)
        f
        (Glsl.float1 g)
        (Glsl.float1 h)
        i


mat31ffff1fff :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat31ffff1fff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        (Glsl.float1 c)
        (Glsl.float1 d)
        (Glsl.float1 e)
        f
        (Glsl.float1 g)
        (Glsl.float1 h)
        (Glsl.float1 i)


mat31fffff111 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat31fffff111 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        (Glsl.float1 c)
        (Glsl.float1 d)
        (Glsl.float1 e)
        (Glsl.float1 f)
        g
        h
        i


mat31fffff11f :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat31fffff11f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        (Glsl.float1 c)
        (Glsl.float1 d)
        (Glsl.float1 e)
        (Glsl.float1 f)
        g
        h
        (Glsl.float1 i)


mat31fffff1f1 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat31fffff1f1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        (Glsl.float1 c)
        (Glsl.float1 d)
        (Glsl.float1 e)
        (Glsl.float1 f)
        g
        (Glsl.float1 h)
        i


mat31fffff1ff :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat31fffff1ff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        (Glsl.float1 c)
        (Glsl.float1 d)
        (Glsl.float1 e)
        (Glsl.float1 f)
        g
        (Glsl.float1 h)
        (Glsl.float1 i)


mat31ffffff11 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat31ffffff11 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        (Glsl.float1 c)
        (Glsl.float1 d)
        (Glsl.float1 e)
        (Glsl.float1 f)
        (Glsl.float1 g)
        h
        i


mat31ffffff1f :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat31ffffff1f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        (Glsl.float1 c)
        (Glsl.float1 d)
        (Glsl.float1 e)
        (Glsl.float1 f)
        (Glsl.float1 g)
        h
        (Glsl.float1 i)


mat31fffffff1 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat31fffffff1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        (Glsl.float1 c)
        (Glsl.float1 d)
        (Glsl.float1 e)
        (Glsl.float1 f)
        (Glsl.float1 g)
        (Glsl.float1 h)
        i


mat31ffffffff :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat31ffffffff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        a
        (Glsl.float1 b)
        (Glsl.float1 c)
        (Glsl.float1 d)
        (Glsl.float1 e)
        (Glsl.float1 f)
        (Glsl.float1 g)
        (Glsl.float1 h)
        (Glsl.float1 i)


mat3333 :
    Glsl.Expression Glsl.Vec3
    -> Glsl.Expression Glsl.Vec3
    -> Glsl.Expression Glsl.Vec3
    -> Glsl.Expression Glsl.Mat3
mat3333 a b c =
    Glsl.unsafeCall3 "mat3" [] a b c


mat3f11111111 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3f11111111 a b c d e f g h i =
    Glsl.unsafeCall9 "mat3" [] (Glsl.float1 a) b c d e f g h i


mat3f1111111f :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3f1111111f a b c d e f g h i =
    Glsl.unsafeCall9 "mat3" [] (Glsl.float1 a) b c d e f g h (Glsl.float1 i)


mat3f111111f1 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3f111111f1 a b c d e f g h i =
    Glsl.unsafeCall9 "mat3" [] (Glsl.float1 a) b c d e f g (Glsl.float1 h) i


mat3f111111ff :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3f111111ff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        c
        d
        e
        f
        g
        (Glsl.float1 h)
        (Glsl.float1 i)


mat3f11111f11 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3f11111f11 a b c d e f g h i =
    Glsl.unsafeCall9 "mat3" [] (Glsl.float1 a) b c d e f (Glsl.float1 g) h i


mat3f11111f1f :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3f11111f1f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        c
        d
        e
        f
        (Glsl.float1 g)
        h
        (Glsl.float1 i)


mat3f11111ff1 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3f11111ff1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        c
        d
        e
        f
        (Glsl.float1 g)
        (Glsl.float1 h)
        i


mat3f11111fff :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3f11111fff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        c
        d
        e
        f
        (Glsl.float1 g)
        (Glsl.float1 h)
        (Glsl.float1 i)


mat3f1111f111 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3f1111f111 a b c d e f g h i =
    Glsl.unsafeCall9 "mat3" [] (Glsl.float1 a) b c d e (Glsl.float1 f) g h i


mat3f1111f11f :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3f1111f11f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        c
        d
        e
        (Glsl.float1 f)
        g
        h
        (Glsl.float1 i)


mat3f1111f1f1 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3f1111f1f1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        c
        d
        e
        (Glsl.float1 f)
        g
        (Glsl.float1 h)
        i


mat3f1111f1ff :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3f1111f1ff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        c
        d
        e
        (Glsl.float1 f)
        g
        (Glsl.float1 h)
        (Glsl.float1 i)


mat3f1111ff11 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3f1111ff11 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        c
        d
        e
        (Glsl.float1 f)
        (Glsl.float1 g)
        h
        i


mat3f1111ff1f :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3f1111ff1f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        c
        d
        e
        (Glsl.float1 f)
        (Glsl.float1 g)
        h
        (Glsl.float1 i)


mat3f1111fff1 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3f1111fff1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        c
        d
        e
        (Glsl.float1 f)
        (Glsl.float1 g)
        (Glsl.float1 h)
        i


mat3f1111ffff :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3f1111ffff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        c
        d
        e
        (Glsl.float1 f)
        (Glsl.float1 g)
        (Glsl.float1 h)
        (Glsl.float1 i)


mat3f111f1111 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3f111f1111 a b c d e f g h i =
    Glsl.unsafeCall9 "mat3" [] (Glsl.float1 a) b c d (Glsl.float1 e) f g h i


mat3f111f111f :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3f111f111f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        c
        d
        (Glsl.float1 e)
        f
        g
        h
        (Glsl.float1 i)


mat3f111f11f1 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3f111f11f1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        c
        d
        (Glsl.float1 e)
        f
        g
        (Glsl.float1 h)
        i


mat3f111f11ff :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3f111f11ff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        c
        d
        (Glsl.float1 e)
        f
        g
        (Glsl.float1 h)
        (Glsl.float1 i)


mat3f111f1f11 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3f111f1f11 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        c
        d
        (Glsl.float1 e)
        f
        (Glsl.float1 g)
        h
        i


mat3f111f1f1f :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3f111f1f1f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        c
        d
        (Glsl.float1 e)
        f
        (Glsl.float1 g)
        h
        (Glsl.float1 i)


mat3f111f1ff1 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3f111f1ff1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        c
        d
        (Glsl.float1 e)
        f
        (Glsl.float1 g)
        (Glsl.float1 h)
        i


mat3f111f1fff :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3f111f1fff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        c
        d
        (Glsl.float1 e)
        f
        (Glsl.float1 g)
        (Glsl.float1 h)
        (Glsl.float1 i)


mat3f111ff111 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3f111ff111 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        c
        d
        (Glsl.float1 e)
        (Glsl.float1 f)
        g
        h
        i


mat3f111ff11f :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3f111ff11f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        c
        d
        (Glsl.float1 e)
        (Glsl.float1 f)
        g
        h
        (Glsl.float1 i)


mat3f111ff1f1 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3f111ff1f1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        c
        d
        (Glsl.float1 e)
        (Glsl.float1 f)
        g
        (Glsl.float1 h)
        i


mat3f111ff1ff :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3f111ff1ff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        c
        d
        (Glsl.float1 e)
        (Glsl.float1 f)
        g
        (Glsl.float1 h)
        (Glsl.float1 i)


mat3f111fff11 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3f111fff11 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        c
        d
        (Glsl.float1 e)
        (Glsl.float1 f)
        (Glsl.float1 g)
        h
        i


mat3f111fff1f :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3f111fff1f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        c
        d
        (Glsl.float1 e)
        (Glsl.float1 f)
        (Glsl.float1 g)
        h
        (Glsl.float1 i)


mat3f111ffff1 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3f111ffff1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        c
        d
        (Glsl.float1 e)
        (Glsl.float1 f)
        (Glsl.float1 g)
        (Glsl.float1 h)
        i


mat3f111fffff :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3f111fffff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        c
        d
        (Glsl.float1 e)
        (Glsl.float1 f)
        (Glsl.float1 g)
        (Glsl.float1 h)
        (Glsl.float1 i)


mat3f11f11111 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3f11f11111 a b c d e f g h i =
    Glsl.unsafeCall9 "mat3" [] (Glsl.float1 a) b c (Glsl.float1 d) e f g h i


mat3f11f1111f :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3f11f1111f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        c
        (Glsl.float1 d)
        e
        f
        g
        h
        (Glsl.float1 i)


mat3f11f111f1 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3f11f111f1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        c
        (Glsl.float1 d)
        e
        f
        g
        (Glsl.float1 h)
        i


mat3f11f111ff :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3f11f111ff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        c
        (Glsl.float1 d)
        e
        f
        g
        (Glsl.float1 h)
        (Glsl.float1 i)


mat3f11f11f11 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3f11f11f11 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        c
        (Glsl.float1 d)
        e
        f
        (Glsl.float1 g)
        h
        i


mat3f11f11f1f :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3f11f11f1f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        c
        (Glsl.float1 d)
        e
        f
        (Glsl.float1 g)
        h
        (Glsl.float1 i)


mat3f11f11ff1 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3f11f11ff1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        c
        (Glsl.float1 d)
        e
        f
        (Glsl.float1 g)
        (Glsl.float1 h)
        i


mat3f11f11fff :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3f11f11fff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        c
        (Glsl.float1 d)
        e
        f
        (Glsl.float1 g)
        (Glsl.float1 h)
        (Glsl.float1 i)


mat3f11f1f111 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3f11f1f111 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        c
        (Glsl.float1 d)
        e
        (Glsl.float1 f)
        g
        h
        i


mat3f11f1f11f :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3f11f1f11f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        c
        (Glsl.float1 d)
        e
        (Glsl.float1 f)
        g
        h
        (Glsl.float1 i)


mat3f11f1f1f1 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3f11f1f1f1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        c
        (Glsl.float1 d)
        e
        (Glsl.float1 f)
        g
        (Glsl.float1 h)
        i


mat3f11f1f1ff :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3f11f1f1ff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        c
        (Glsl.float1 d)
        e
        (Glsl.float1 f)
        g
        (Glsl.float1 h)
        (Glsl.float1 i)


mat3f11f1ff11 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3f11f1ff11 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        c
        (Glsl.float1 d)
        e
        (Glsl.float1 f)
        (Glsl.float1 g)
        h
        i


mat3f11f1ff1f :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3f11f1ff1f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        c
        (Glsl.float1 d)
        e
        (Glsl.float1 f)
        (Glsl.float1 g)
        h
        (Glsl.float1 i)


mat3f11f1fff1 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3f11f1fff1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        c
        (Glsl.float1 d)
        e
        (Glsl.float1 f)
        (Glsl.float1 g)
        (Glsl.float1 h)
        i


mat3f11f1ffff :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3f11f1ffff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        c
        (Glsl.float1 d)
        e
        (Glsl.float1 f)
        (Glsl.float1 g)
        (Glsl.float1 h)
        (Glsl.float1 i)


mat3f11ff1111 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3f11ff1111 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        c
        (Glsl.float1 d)
        (Glsl.float1 e)
        f
        g
        h
        i


mat3f11ff111f :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3f11ff111f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        c
        (Glsl.float1 d)
        (Glsl.float1 e)
        f
        g
        h
        (Glsl.float1 i)


mat3f11ff11f1 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3f11ff11f1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        c
        (Glsl.float1 d)
        (Glsl.float1 e)
        f
        g
        (Glsl.float1 h)
        i


mat3f11ff11ff :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3f11ff11ff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        c
        (Glsl.float1 d)
        (Glsl.float1 e)
        f
        g
        (Glsl.float1 h)
        (Glsl.float1 i)


mat3f11ff1f11 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3f11ff1f11 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        c
        (Glsl.float1 d)
        (Glsl.float1 e)
        f
        (Glsl.float1 g)
        h
        i


mat3f11ff1f1f :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3f11ff1f1f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        c
        (Glsl.float1 d)
        (Glsl.float1 e)
        f
        (Glsl.float1 g)
        h
        (Glsl.float1 i)


mat3f11ff1ff1 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3f11ff1ff1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        c
        (Glsl.float1 d)
        (Glsl.float1 e)
        f
        (Glsl.float1 g)
        (Glsl.float1 h)
        i


mat3f11ff1fff :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3f11ff1fff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        c
        (Glsl.float1 d)
        (Glsl.float1 e)
        f
        (Glsl.float1 g)
        (Glsl.float1 h)
        (Glsl.float1 i)


mat3f11fff111 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3f11fff111 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        c
        (Glsl.float1 d)
        (Glsl.float1 e)
        (Glsl.float1 f)
        g
        h
        i


mat3f11fff11f :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3f11fff11f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        c
        (Glsl.float1 d)
        (Glsl.float1 e)
        (Glsl.float1 f)
        g
        h
        (Glsl.float1 i)


mat3f11fff1f1 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3f11fff1f1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        c
        (Glsl.float1 d)
        (Glsl.float1 e)
        (Glsl.float1 f)
        g
        (Glsl.float1 h)
        i


mat3f11fff1ff :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3f11fff1ff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        c
        (Glsl.float1 d)
        (Glsl.float1 e)
        (Glsl.float1 f)
        g
        (Glsl.float1 h)
        (Glsl.float1 i)


mat3f11ffff11 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3f11ffff11 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        c
        (Glsl.float1 d)
        (Glsl.float1 e)
        (Glsl.float1 f)
        (Glsl.float1 g)
        h
        i


mat3f11ffff1f :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3f11ffff1f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        c
        (Glsl.float1 d)
        (Glsl.float1 e)
        (Glsl.float1 f)
        (Glsl.float1 g)
        h
        (Glsl.float1 i)


mat3f11fffff1 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3f11fffff1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        c
        (Glsl.float1 d)
        (Glsl.float1 e)
        (Glsl.float1 f)
        (Glsl.float1 g)
        (Glsl.float1 h)
        i


mat3f11ffffff :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3f11ffffff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        c
        (Glsl.float1 d)
        (Glsl.float1 e)
        (Glsl.float1 f)
        (Glsl.float1 g)
        (Glsl.float1 h)
        (Glsl.float1 i)


mat3f1f111111 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3f1f111111 a b c d e f g h i =
    Glsl.unsafeCall9 "mat3" [] (Glsl.float1 a) b (Glsl.float1 c) d e f g h i


mat3f1f11111f :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3f1f11111f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        (Glsl.float1 c)
        d
        e
        f
        g
        h
        (Glsl.float1 i)


mat3f1f1111f1 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3f1f1111f1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        (Glsl.float1 c)
        d
        e
        f
        g
        (Glsl.float1 h)
        i


mat3f1f1111ff :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3f1f1111ff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        (Glsl.float1 c)
        d
        e
        f
        g
        (Glsl.float1 h)
        (Glsl.float1 i)


mat3f1f111f11 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3f1f111f11 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        (Glsl.float1 c)
        d
        e
        f
        (Glsl.float1 g)
        h
        i


mat3f1f111f1f :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3f1f111f1f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        (Glsl.float1 c)
        d
        e
        f
        (Glsl.float1 g)
        h
        (Glsl.float1 i)


mat3f1f111ff1 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3f1f111ff1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        (Glsl.float1 c)
        d
        e
        f
        (Glsl.float1 g)
        (Glsl.float1 h)
        i


mat3f1f111fff :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3f1f111fff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        (Glsl.float1 c)
        d
        e
        f
        (Glsl.float1 g)
        (Glsl.float1 h)
        (Glsl.float1 i)


mat3f1f11f111 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3f1f11f111 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        (Glsl.float1 c)
        d
        e
        (Glsl.float1 f)
        g
        h
        i


mat3f1f11f11f :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3f1f11f11f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        (Glsl.float1 c)
        d
        e
        (Glsl.float1 f)
        g
        h
        (Glsl.float1 i)


mat3f1f11f1f1 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3f1f11f1f1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        (Glsl.float1 c)
        d
        e
        (Glsl.float1 f)
        g
        (Glsl.float1 h)
        i


mat3f1f11f1ff :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3f1f11f1ff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        (Glsl.float1 c)
        d
        e
        (Glsl.float1 f)
        g
        (Glsl.float1 h)
        (Glsl.float1 i)


mat3f1f11ff11 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3f1f11ff11 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        (Glsl.float1 c)
        d
        e
        (Glsl.float1 f)
        (Glsl.float1 g)
        h
        i


mat3f1f11ff1f :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3f1f11ff1f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        (Glsl.float1 c)
        d
        e
        (Glsl.float1 f)
        (Glsl.float1 g)
        h
        (Glsl.float1 i)


mat3f1f11fff1 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3f1f11fff1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        (Glsl.float1 c)
        d
        e
        (Glsl.float1 f)
        (Glsl.float1 g)
        (Glsl.float1 h)
        i


mat3f1f11ffff :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3f1f11ffff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        (Glsl.float1 c)
        d
        e
        (Glsl.float1 f)
        (Glsl.float1 g)
        (Glsl.float1 h)
        (Glsl.float1 i)


mat3f1f1f1111 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3f1f1f1111 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        (Glsl.float1 c)
        d
        (Glsl.float1 e)
        f
        g
        h
        i


mat3f1f1f111f :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3f1f1f111f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        (Glsl.float1 c)
        d
        (Glsl.float1 e)
        f
        g
        h
        (Glsl.float1 i)


mat3f1f1f11f1 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3f1f1f11f1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        (Glsl.float1 c)
        d
        (Glsl.float1 e)
        f
        g
        (Glsl.float1 h)
        i


mat3f1f1f11ff :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3f1f1f11ff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        (Glsl.float1 c)
        d
        (Glsl.float1 e)
        f
        g
        (Glsl.float1 h)
        (Glsl.float1 i)


mat3f1f1f1f11 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3f1f1f1f11 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        (Glsl.float1 c)
        d
        (Glsl.float1 e)
        f
        (Glsl.float1 g)
        h
        i


mat3f1f1f1f1f :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3f1f1f1f1f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        (Glsl.float1 c)
        d
        (Glsl.float1 e)
        f
        (Glsl.float1 g)
        h
        (Glsl.float1 i)


mat3f1f1f1ff1 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3f1f1f1ff1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        (Glsl.float1 c)
        d
        (Glsl.float1 e)
        f
        (Glsl.float1 g)
        (Glsl.float1 h)
        i


mat3f1f1f1fff :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3f1f1f1fff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        (Glsl.float1 c)
        d
        (Glsl.float1 e)
        f
        (Glsl.float1 g)
        (Glsl.float1 h)
        (Glsl.float1 i)


mat3f1f1ff111 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3f1f1ff111 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        (Glsl.float1 c)
        d
        (Glsl.float1 e)
        (Glsl.float1 f)
        g
        h
        i


mat3f1f1ff11f :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3f1f1ff11f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        (Glsl.float1 c)
        d
        (Glsl.float1 e)
        (Glsl.float1 f)
        g
        h
        (Glsl.float1 i)


mat3f1f1ff1f1 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3f1f1ff1f1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        (Glsl.float1 c)
        d
        (Glsl.float1 e)
        (Glsl.float1 f)
        g
        (Glsl.float1 h)
        i


mat3f1f1ff1ff :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3f1f1ff1ff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        (Glsl.float1 c)
        d
        (Glsl.float1 e)
        (Glsl.float1 f)
        g
        (Glsl.float1 h)
        (Glsl.float1 i)


mat3f1f1fff11 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3f1f1fff11 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        (Glsl.float1 c)
        d
        (Glsl.float1 e)
        (Glsl.float1 f)
        (Glsl.float1 g)
        h
        i


mat3f1f1fff1f :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3f1f1fff1f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        (Glsl.float1 c)
        d
        (Glsl.float1 e)
        (Glsl.float1 f)
        (Glsl.float1 g)
        h
        (Glsl.float1 i)


mat3f1f1ffff1 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3f1f1ffff1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        (Glsl.float1 c)
        d
        (Glsl.float1 e)
        (Glsl.float1 f)
        (Glsl.float1 g)
        (Glsl.float1 h)
        i


mat3f1f1fffff :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3f1f1fffff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        (Glsl.float1 c)
        d
        (Glsl.float1 e)
        (Glsl.float1 f)
        (Glsl.float1 g)
        (Glsl.float1 h)
        (Glsl.float1 i)


mat3f1ff11111 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3f1ff11111 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        (Glsl.float1 c)
        (Glsl.float1 d)
        e
        f
        g
        h
        i


mat3f1ff1111f :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3f1ff1111f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        (Glsl.float1 c)
        (Glsl.float1 d)
        e
        f
        g
        h
        (Glsl.float1 i)


mat3f1ff111f1 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3f1ff111f1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        (Glsl.float1 c)
        (Glsl.float1 d)
        e
        f
        g
        (Glsl.float1 h)
        i


mat3f1ff111ff :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3f1ff111ff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        (Glsl.float1 c)
        (Glsl.float1 d)
        e
        f
        g
        (Glsl.float1 h)
        (Glsl.float1 i)


mat3f1ff11f11 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3f1ff11f11 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        (Glsl.float1 c)
        (Glsl.float1 d)
        e
        f
        (Glsl.float1 g)
        h
        i


mat3f1ff11f1f :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3f1ff11f1f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        (Glsl.float1 c)
        (Glsl.float1 d)
        e
        f
        (Glsl.float1 g)
        h
        (Glsl.float1 i)


mat3f1ff11ff1 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3f1ff11ff1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        (Glsl.float1 c)
        (Glsl.float1 d)
        e
        f
        (Glsl.float1 g)
        (Glsl.float1 h)
        i


mat3f1ff11fff :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3f1ff11fff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        (Glsl.float1 c)
        (Glsl.float1 d)
        e
        f
        (Glsl.float1 g)
        (Glsl.float1 h)
        (Glsl.float1 i)


mat3f1ff1f111 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3f1ff1f111 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        (Glsl.float1 c)
        (Glsl.float1 d)
        e
        (Glsl.float1 f)
        g
        h
        i


mat3f1ff1f11f :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3f1ff1f11f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        (Glsl.float1 c)
        (Glsl.float1 d)
        e
        (Glsl.float1 f)
        g
        h
        (Glsl.float1 i)


mat3f1ff1f1f1 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3f1ff1f1f1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        (Glsl.float1 c)
        (Glsl.float1 d)
        e
        (Glsl.float1 f)
        g
        (Glsl.float1 h)
        i


mat3f1ff1f1ff :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3f1ff1f1ff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        (Glsl.float1 c)
        (Glsl.float1 d)
        e
        (Glsl.float1 f)
        g
        (Glsl.float1 h)
        (Glsl.float1 i)


mat3f1ff1ff11 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3f1ff1ff11 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        (Glsl.float1 c)
        (Glsl.float1 d)
        e
        (Glsl.float1 f)
        (Glsl.float1 g)
        h
        i


mat3f1ff1ff1f :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3f1ff1ff1f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        (Glsl.float1 c)
        (Glsl.float1 d)
        e
        (Glsl.float1 f)
        (Glsl.float1 g)
        h
        (Glsl.float1 i)


mat3f1ff1fff1 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3f1ff1fff1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        (Glsl.float1 c)
        (Glsl.float1 d)
        e
        (Glsl.float1 f)
        (Glsl.float1 g)
        (Glsl.float1 h)
        i


mat3f1ff1ffff :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3f1ff1ffff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        (Glsl.float1 c)
        (Glsl.float1 d)
        e
        (Glsl.float1 f)
        (Glsl.float1 g)
        (Glsl.float1 h)
        (Glsl.float1 i)


mat3f1fff1111 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3f1fff1111 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        (Glsl.float1 c)
        (Glsl.float1 d)
        (Glsl.float1 e)
        f
        g
        h
        i


mat3f1fff111f :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3f1fff111f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        (Glsl.float1 c)
        (Glsl.float1 d)
        (Glsl.float1 e)
        f
        g
        h
        (Glsl.float1 i)


mat3f1fff11f1 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3f1fff11f1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        (Glsl.float1 c)
        (Glsl.float1 d)
        (Glsl.float1 e)
        f
        g
        (Glsl.float1 h)
        i


mat3f1fff11ff :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3f1fff11ff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        (Glsl.float1 c)
        (Glsl.float1 d)
        (Glsl.float1 e)
        f
        g
        (Glsl.float1 h)
        (Glsl.float1 i)


mat3f1fff1f11 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3f1fff1f11 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        (Glsl.float1 c)
        (Glsl.float1 d)
        (Glsl.float1 e)
        f
        (Glsl.float1 g)
        h
        i


mat3f1fff1f1f :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3f1fff1f1f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        (Glsl.float1 c)
        (Glsl.float1 d)
        (Glsl.float1 e)
        f
        (Glsl.float1 g)
        h
        (Glsl.float1 i)


mat3f1fff1ff1 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3f1fff1ff1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        (Glsl.float1 c)
        (Glsl.float1 d)
        (Glsl.float1 e)
        f
        (Glsl.float1 g)
        (Glsl.float1 h)
        i


mat3f1fff1fff :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3f1fff1fff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        (Glsl.float1 c)
        (Glsl.float1 d)
        (Glsl.float1 e)
        f
        (Glsl.float1 g)
        (Glsl.float1 h)
        (Glsl.float1 i)


mat3f1ffff111 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3f1ffff111 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        (Glsl.float1 c)
        (Glsl.float1 d)
        (Glsl.float1 e)
        (Glsl.float1 f)
        g
        h
        i


mat3f1ffff11f :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3f1ffff11f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        (Glsl.float1 c)
        (Glsl.float1 d)
        (Glsl.float1 e)
        (Glsl.float1 f)
        g
        h
        (Glsl.float1 i)


mat3f1ffff1f1 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3f1ffff1f1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        (Glsl.float1 c)
        (Glsl.float1 d)
        (Glsl.float1 e)
        (Glsl.float1 f)
        g
        (Glsl.float1 h)
        i


mat3f1ffff1ff :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3f1ffff1ff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        (Glsl.float1 c)
        (Glsl.float1 d)
        (Glsl.float1 e)
        (Glsl.float1 f)
        g
        (Glsl.float1 h)
        (Glsl.float1 i)


mat3f1fffff11 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3f1fffff11 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        (Glsl.float1 c)
        (Glsl.float1 d)
        (Glsl.float1 e)
        (Glsl.float1 f)
        (Glsl.float1 g)
        h
        i


mat3f1fffff1f :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3f1fffff1f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        (Glsl.float1 c)
        (Glsl.float1 d)
        (Glsl.float1 e)
        (Glsl.float1 f)
        (Glsl.float1 g)
        h
        (Glsl.float1 i)


mat3f1ffffff1 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3f1ffffff1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        (Glsl.float1 c)
        (Glsl.float1 d)
        (Glsl.float1 e)
        (Glsl.float1 f)
        (Glsl.float1 g)
        (Glsl.float1 h)
        i


mat3f1fffffff :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3f1fffffff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        b
        (Glsl.float1 c)
        (Glsl.float1 d)
        (Glsl.float1 e)
        (Glsl.float1 f)
        (Glsl.float1 g)
        (Glsl.float1 h)
        (Glsl.float1 i)


mat3ff1111111 :
    Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3ff1111111 a b c d e f g h i =
    Glsl.unsafeCall9 "mat3" [] (Glsl.float1 a) (Glsl.float1 b) c d e f g h i


mat3ff111111f :
    Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3ff111111f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        c
        d
        e
        f
        g
        h
        (Glsl.float1 i)


mat3ff11111f1 :
    Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3ff11111f1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        c
        d
        e
        f
        g
        (Glsl.float1 h)
        i


mat3ff11111ff :
    Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3ff11111ff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        c
        d
        e
        f
        g
        (Glsl.float1 h)
        (Glsl.float1 i)


mat3ff1111f11 :
    Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3ff1111f11 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        c
        d
        e
        f
        (Glsl.float1 g)
        h
        i


mat3ff1111f1f :
    Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3ff1111f1f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        c
        d
        e
        f
        (Glsl.float1 g)
        h
        (Glsl.float1 i)


mat3ff1111ff1 :
    Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3ff1111ff1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        c
        d
        e
        f
        (Glsl.float1 g)
        (Glsl.float1 h)
        i


mat3ff1111fff :
    Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3ff1111fff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        c
        d
        e
        f
        (Glsl.float1 g)
        (Glsl.float1 h)
        (Glsl.float1 i)


mat3ff111f111 :
    Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3ff111f111 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        c
        d
        e
        (Glsl.float1 f)
        g
        h
        i


mat3ff111f11f :
    Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3ff111f11f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        c
        d
        e
        (Glsl.float1 f)
        g
        h
        (Glsl.float1 i)


mat3ff111f1f1 :
    Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3ff111f1f1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        c
        d
        e
        (Glsl.float1 f)
        g
        (Glsl.float1 h)
        i


mat3ff111f1ff :
    Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3ff111f1ff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        c
        d
        e
        (Glsl.float1 f)
        g
        (Glsl.float1 h)
        (Glsl.float1 i)


mat3ff111ff11 :
    Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3ff111ff11 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        c
        d
        e
        (Glsl.float1 f)
        (Glsl.float1 g)
        h
        i


mat3ff111ff1f :
    Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3ff111ff1f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        c
        d
        e
        (Glsl.float1 f)
        (Glsl.float1 g)
        h
        (Glsl.float1 i)


mat3ff111fff1 :
    Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3ff111fff1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        c
        d
        e
        (Glsl.float1 f)
        (Glsl.float1 g)
        (Glsl.float1 h)
        i


mat3ff111ffff :
    Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3ff111ffff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        c
        d
        e
        (Glsl.float1 f)
        (Glsl.float1 g)
        (Glsl.float1 h)
        (Glsl.float1 i)


mat3ff11f1111 :
    Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3ff11f1111 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        c
        d
        (Glsl.float1 e)
        f
        g
        h
        i


mat3ff11f111f :
    Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3ff11f111f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        c
        d
        (Glsl.float1 e)
        f
        g
        h
        (Glsl.float1 i)


mat3ff11f11f1 :
    Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3ff11f11f1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        c
        d
        (Glsl.float1 e)
        f
        g
        (Glsl.float1 h)
        i


mat3ff11f11ff :
    Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3ff11f11ff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        c
        d
        (Glsl.float1 e)
        f
        g
        (Glsl.float1 h)
        (Glsl.float1 i)


mat3ff11f1f11 :
    Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3ff11f1f11 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        c
        d
        (Glsl.float1 e)
        f
        (Glsl.float1 g)
        h
        i


mat3ff11f1f1f :
    Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3ff11f1f1f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        c
        d
        (Glsl.float1 e)
        f
        (Glsl.float1 g)
        h
        (Glsl.float1 i)


mat3ff11f1ff1 :
    Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3ff11f1ff1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        c
        d
        (Glsl.float1 e)
        f
        (Glsl.float1 g)
        (Glsl.float1 h)
        i


mat3ff11f1fff :
    Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3ff11f1fff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        c
        d
        (Glsl.float1 e)
        f
        (Glsl.float1 g)
        (Glsl.float1 h)
        (Glsl.float1 i)


mat3ff11ff111 :
    Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3ff11ff111 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        c
        d
        (Glsl.float1 e)
        (Glsl.float1 f)
        g
        h
        i


mat3ff11ff11f :
    Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3ff11ff11f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        c
        d
        (Glsl.float1 e)
        (Glsl.float1 f)
        g
        h
        (Glsl.float1 i)


mat3ff11ff1f1 :
    Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3ff11ff1f1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        c
        d
        (Glsl.float1 e)
        (Glsl.float1 f)
        g
        (Glsl.float1 h)
        i


mat3ff11ff1ff :
    Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3ff11ff1ff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        c
        d
        (Glsl.float1 e)
        (Glsl.float1 f)
        g
        (Glsl.float1 h)
        (Glsl.float1 i)


mat3ff11fff11 :
    Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3ff11fff11 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        c
        d
        (Glsl.float1 e)
        (Glsl.float1 f)
        (Glsl.float1 g)
        h
        i


mat3ff11fff1f :
    Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3ff11fff1f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        c
        d
        (Glsl.float1 e)
        (Glsl.float1 f)
        (Glsl.float1 g)
        h
        (Glsl.float1 i)


mat3ff11ffff1 :
    Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3ff11ffff1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        c
        d
        (Glsl.float1 e)
        (Glsl.float1 f)
        (Glsl.float1 g)
        (Glsl.float1 h)
        i


mat3ff11fffff :
    Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3ff11fffff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        c
        d
        (Glsl.float1 e)
        (Glsl.float1 f)
        (Glsl.float1 g)
        (Glsl.float1 h)
        (Glsl.float1 i)


mat3ff1f11111 :
    Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3ff1f11111 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        c
        (Glsl.float1 d)
        e
        f
        g
        h
        i


mat3ff1f1111f :
    Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3ff1f1111f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        c
        (Glsl.float1 d)
        e
        f
        g
        h
        (Glsl.float1 i)


mat3ff1f111f1 :
    Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3ff1f111f1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        c
        (Glsl.float1 d)
        e
        f
        g
        (Glsl.float1 h)
        i


mat3ff1f111ff :
    Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3ff1f111ff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        c
        (Glsl.float1 d)
        e
        f
        g
        (Glsl.float1 h)
        (Glsl.float1 i)


mat3ff1f11f11 :
    Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3ff1f11f11 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        c
        (Glsl.float1 d)
        e
        f
        (Glsl.float1 g)
        h
        i


mat3ff1f11f1f :
    Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3ff1f11f1f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        c
        (Glsl.float1 d)
        e
        f
        (Glsl.float1 g)
        h
        (Glsl.float1 i)


mat3ff1f11ff1 :
    Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3ff1f11ff1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        c
        (Glsl.float1 d)
        e
        f
        (Glsl.float1 g)
        (Glsl.float1 h)
        i


mat3ff1f11fff :
    Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3ff1f11fff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        c
        (Glsl.float1 d)
        e
        f
        (Glsl.float1 g)
        (Glsl.float1 h)
        (Glsl.float1 i)


mat3ff1f1f111 :
    Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3ff1f1f111 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        c
        (Glsl.float1 d)
        e
        (Glsl.float1 f)
        g
        h
        i


mat3ff1f1f11f :
    Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3ff1f1f11f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        c
        (Glsl.float1 d)
        e
        (Glsl.float1 f)
        g
        h
        (Glsl.float1 i)


mat3ff1f1f1f1 :
    Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3ff1f1f1f1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        c
        (Glsl.float1 d)
        e
        (Glsl.float1 f)
        g
        (Glsl.float1 h)
        i


mat3ff1f1f1ff :
    Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3ff1f1f1ff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        c
        (Glsl.float1 d)
        e
        (Glsl.float1 f)
        g
        (Glsl.float1 h)
        (Glsl.float1 i)


mat3ff1f1ff11 :
    Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3ff1f1ff11 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        c
        (Glsl.float1 d)
        e
        (Glsl.float1 f)
        (Glsl.float1 g)
        h
        i


mat3ff1f1ff1f :
    Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3ff1f1ff1f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        c
        (Glsl.float1 d)
        e
        (Glsl.float1 f)
        (Glsl.float1 g)
        h
        (Glsl.float1 i)


mat3ff1f1fff1 :
    Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3ff1f1fff1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        c
        (Glsl.float1 d)
        e
        (Glsl.float1 f)
        (Glsl.float1 g)
        (Glsl.float1 h)
        i


mat3ff1f1ffff :
    Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3ff1f1ffff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        c
        (Glsl.float1 d)
        e
        (Glsl.float1 f)
        (Glsl.float1 g)
        (Glsl.float1 h)
        (Glsl.float1 i)


mat3ff1ff1111 :
    Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3ff1ff1111 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        c
        (Glsl.float1 d)
        (Glsl.float1 e)
        f
        g
        h
        i


mat3ff1ff111f :
    Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3ff1ff111f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        c
        (Glsl.float1 d)
        (Glsl.float1 e)
        f
        g
        h
        (Glsl.float1 i)


mat3ff1ff11f1 :
    Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3ff1ff11f1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        c
        (Glsl.float1 d)
        (Glsl.float1 e)
        f
        g
        (Glsl.float1 h)
        i


mat3ff1ff11ff :
    Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3ff1ff11ff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        c
        (Glsl.float1 d)
        (Glsl.float1 e)
        f
        g
        (Glsl.float1 h)
        (Glsl.float1 i)


mat3ff1ff1f11 :
    Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3ff1ff1f11 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        c
        (Glsl.float1 d)
        (Glsl.float1 e)
        f
        (Glsl.float1 g)
        h
        i


mat3ff1ff1f1f :
    Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3ff1ff1f1f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        c
        (Glsl.float1 d)
        (Glsl.float1 e)
        f
        (Glsl.float1 g)
        h
        (Glsl.float1 i)


mat3ff1ff1ff1 :
    Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3ff1ff1ff1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        c
        (Glsl.float1 d)
        (Glsl.float1 e)
        f
        (Glsl.float1 g)
        (Glsl.float1 h)
        i


mat3ff1ff1fff :
    Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3ff1ff1fff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        c
        (Glsl.float1 d)
        (Glsl.float1 e)
        f
        (Glsl.float1 g)
        (Glsl.float1 h)
        (Glsl.float1 i)


mat3ff1fff111 :
    Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3ff1fff111 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        c
        (Glsl.float1 d)
        (Glsl.float1 e)
        (Glsl.float1 f)
        g
        h
        i


mat3ff1fff11f :
    Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3ff1fff11f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        c
        (Glsl.float1 d)
        (Glsl.float1 e)
        (Glsl.float1 f)
        g
        h
        (Glsl.float1 i)


mat3ff1fff1f1 :
    Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3ff1fff1f1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        c
        (Glsl.float1 d)
        (Glsl.float1 e)
        (Glsl.float1 f)
        g
        (Glsl.float1 h)
        i


mat3ff1fff1ff :
    Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3ff1fff1ff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        c
        (Glsl.float1 d)
        (Glsl.float1 e)
        (Glsl.float1 f)
        g
        (Glsl.float1 h)
        (Glsl.float1 i)


mat3ff1ffff11 :
    Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3ff1ffff11 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        c
        (Glsl.float1 d)
        (Glsl.float1 e)
        (Glsl.float1 f)
        (Glsl.float1 g)
        h
        i


mat3ff1ffff1f :
    Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3ff1ffff1f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        c
        (Glsl.float1 d)
        (Glsl.float1 e)
        (Glsl.float1 f)
        (Glsl.float1 g)
        h
        (Glsl.float1 i)


mat3ff1fffff1 :
    Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3ff1fffff1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        c
        (Glsl.float1 d)
        (Glsl.float1 e)
        (Glsl.float1 f)
        (Glsl.float1 g)
        (Glsl.float1 h)
        i


mat3ff1ffffff :
    Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3ff1ffffff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        c
        (Glsl.float1 d)
        (Glsl.float1 e)
        (Glsl.float1 f)
        (Glsl.float1 g)
        (Glsl.float1 h)
        (Glsl.float1 i)


mat3fff111111 :
    Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3fff111111 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        (Glsl.float1 c)
        d
        e
        f
        g
        h
        i


mat3fff11111f :
    Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3fff11111f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        (Glsl.float1 c)
        d
        e
        f
        g
        h
        (Glsl.float1 i)


mat3fff1111f1 :
    Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3fff1111f1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        (Glsl.float1 c)
        d
        e
        f
        g
        (Glsl.float1 h)
        i


mat3fff1111ff :
    Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3fff1111ff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        (Glsl.float1 c)
        d
        e
        f
        g
        (Glsl.float1 h)
        (Glsl.float1 i)


mat3fff111f11 :
    Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3fff111f11 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        (Glsl.float1 c)
        d
        e
        f
        (Glsl.float1 g)
        h
        i


mat3fff111f1f :
    Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3fff111f1f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        (Glsl.float1 c)
        d
        e
        f
        (Glsl.float1 g)
        h
        (Glsl.float1 i)


mat3fff111ff1 :
    Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3fff111ff1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        (Glsl.float1 c)
        d
        e
        f
        (Glsl.float1 g)
        (Glsl.float1 h)
        i


mat3fff111fff :
    Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3fff111fff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        (Glsl.float1 c)
        d
        e
        f
        (Glsl.float1 g)
        (Glsl.float1 h)
        (Glsl.float1 i)


mat3fff11f111 :
    Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3fff11f111 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        (Glsl.float1 c)
        d
        e
        (Glsl.float1 f)
        g
        h
        i


mat3fff11f11f :
    Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3fff11f11f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        (Glsl.float1 c)
        d
        e
        (Glsl.float1 f)
        g
        h
        (Glsl.float1 i)


mat3fff11f1f1 :
    Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3fff11f1f1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        (Glsl.float1 c)
        d
        e
        (Glsl.float1 f)
        g
        (Glsl.float1 h)
        i


mat3fff11f1ff :
    Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3fff11f1ff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        (Glsl.float1 c)
        d
        e
        (Glsl.float1 f)
        g
        (Glsl.float1 h)
        (Glsl.float1 i)


mat3fff11ff11 :
    Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3fff11ff11 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        (Glsl.float1 c)
        d
        e
        (Glsl.float1 f)
        (Glsl.float1 g)
        h
        i


mat3fff11ff1f :
    Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3fff11ff1f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        (Glsl.float1 c)
        d
        e
        (Glsl.float1 f)
        (Glsl.float1 g)
        h
        (Glsl.float1 i)


mat3fff11fff1 :
    Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3fff11fff1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        (Glsl.float1 c)
        d
        e
        (Glsl.float1 f)
        (Glsl.float1 g)
        (Glsl.float1 h)
        i


mat3fff11ffff :
    Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3fff11ffff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        (Glsl.float1 c)
        d
        e
        (Glsl.float1 f)
        (Glsl.float1 g)
        (Glsl.float1 h)
        (Glsl.float1 i)


mat3fff1f1111 :
    Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3fff1f1111 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        (Glsl.float1 c)
        d
        (Glsl.float1 e)
        f
        g
        h
        i


mat3fff1f111f :
    Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3fff1f111f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        (Glsl.float1 c)
        d
        (Glsl.float1 e)
        f
        g
        h
        (Glsl.float1 i)


mat3fff1f11f1 :
    Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3fff1f11f1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        (Glsl.float1 c)
        d
        (Glsl.float1 e)
        f
        g
        (Glsl.float1 h)
        i


mat3fff1f11ff :
    Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3fff1f11ff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        (Glsl.float1 c)
        d
        (Glsl.float1 e)
        f
        g
        (Glsl.float1 h)
        (Glsl.float1 i)


mat3fff1f1f11 :
    Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3fff1f1f11 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        (Glsl.float1 c)
        d
        (Glsl.float1 e)
        f
        (Glsl.float1 g)
        h
        i


mat3fff1f1f1f :
    Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3fff1f1f1f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        (Glsl.float1 c)
        d
        (Glsl.float1 e)
        f
        (Glsl.float1 g)
        h
        (Glsl.float1 i)


mat3fff1f1ff1 :
    Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3fff1f1ff1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        (Glsl.float1 c)
        d
        (Glsl.float1 e)
        f
        (Glsl.float1 g)
        (Glsl.float1 h)
        i


mat3fff1f1fff :
    Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3fff1f1fff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        (Glsl.float1 c)
        d
        (Glsl.float1 e)
        f
        (Glsl.float1 g)
        (Glsl.float1 h)
        (Glsl.float1 i)


mat3fff1ff111 :
    Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3fff1ff111 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        (Glsl.float1 c)
        d
        (Glsl.float1 e)
        (Glsl.float1 f)
        g
        h
        i


mat3fff1ff11f :
    Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3fff1ff11f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        (Glsl.float1 c)
        d
        (Glsl.float1 e)
        (Glsl.float1 f)
        g
        h
        (Glsl.float1 i)


mat3fff1ff1f1 :
    Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3fff1ff1f1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        (Glsl.float1 c)
        d
        (Glsl.float1 e)
        (Glsl.float1 f)
        g
        (Glsl.float1 h)
        i


mat3fff1ff1ff :
    Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3fff1ff1ff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        (Glsl.float1 c)
        d
        (Glsl.float1 e)
        (Glsl.float1 f)
        g
        (Glsl.float1 h)
        (Glsl.float1 i)


mat3fff1fff11 :
    Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3fff1fff11 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        (Glsl.float1 c)
        d
        (Glsl.float1 e)
        (Glsl.float1 f)
        (Glsl.float1 g)
        h
        i


mat3fff1fff1f :
    Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3fff1fff1f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        (Glsl.float1 c)
        d
        (Glsl.float1 e)
        (Glsl.float1 f)
        (Glsl.float1 g)
        h
        (Glsl.float1 i)


mat3fff1ffff1 :
    Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3fff1ffff1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        (Glsl.float1 c)
        d
        (Glsl.float1 e)
        (Glsl.float1 f)
        (Glsl.float1 g)
        (Glsl.float1 h)
        i


mat3fff1fffff :
    Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3fff1fffff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        (Glsl.float1 c)
        d
        (Glsl.float1 e)
        (Glsl.float1 f)
        (Glsl.float1 g)
        (Glsl.float1 h)
        (Glsl.float1 i)


mat3ffff11111 :
    Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3ffff11111 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        (Glsl.float1 c)
        (Glsl.float1 d)
        e
        f
        g
        h
        i


mat3ffff1111f :
    Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3ffff1111f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        (Glsl.float1 c)
        (Glsl.float1 d)
        e
        f
        g
        h
        (Glsl.float1 i)


mat3ffff111f1 :
    Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3ffff111f1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        (Glsl.float1 c)
        (Glsl.float1 d)
        e
        f
        g
        (Glsl.float1 h)
        i


mat3ffff111ff :
    Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3ffff111ff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        (Glsl.float1 c)
        (Glsl.float1 d)
        e
        f
        g
        (Glsl.float1 h)
        (Glsl.float1 i)


mat3ffff11f11 :
    Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3ffff11f11 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        (Glsl.float1 c)
        (Glsl.float1 d)
        e
        f
        (Glsl.float1 g)
        h
        i


mat3ffff11f1f :
    Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3ffff11f1f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        (Glsl.float1 c)
        (Glsl.float1 d)
        e
        f
        (Glsl.float1 g)
        h
        (Glsl.float1 i)


mat3ffff11ff1 :
    Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3ffff11ff1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        (Glsl.float1 c)
        (Glsl.float1 d)
        e
        f
        (Glsl.float1 g)
        (Glsl.float1 h)
        i


mat3ffff11fff :
    Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3ffff11fff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        (Glsl.float1 c)
        (Glsl.float1 d)
        e
        f
        (Glsl.float1 g)
        (Glsl.float1 h)
        (Glsl.float1 i)


mat3ffff1f111 :
    Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3ffff1f111 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        (Glsl.float1 c)
        (Glsl.float1 d)
        e
        (Glsl.float1 f)
        g
        h
        i


mat3ffff1f11f :
    Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3ffff1f11f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        (Glsl.float1 c)
        (Glsl.float1 d)
        e
        (Glsl.float1 f)
        g
        h
        (Glsl.float1 i)


mat3ffff1f1f1 :
    Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3ffff1f1f1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        (Glsl.float1 c)
        (Glsl.float1 d)
        e
        (Glsl.float1 f)
        g
        (Glsl.float1 h)
        i


mat3ffff1f1ff :
    Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3ffff1f1ff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        (Glsl.float1 c)
        (Glsl.float1 d)
        e
        (Glsl.float1 f)
        g
        (Glsl.float1 h)
        (Glsl.float1 i)


mat3ffff1ff11 :
    Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3ffff1ff11 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        (Glsl.float1 c)
        (Glsl.float1 d)
        e
        (Glsl.float1 f)
        (Glsl.float1 g)
        h
        i


mat3ffff1ff1f :
    Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3ffff1ff1f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        (Glsl.float1 c)
        (Glsl.float1 d)
        e
        (Glsl.float1 f)
        (Glsl.float1 g)
        h
        (Glsl.float1 i)


mat3ffff1fff1 :
    Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3ffff1fff1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        (Glsl.float1 c)
        (Glsl.float1 d)
        e
        (Glsl.float1 f)
        (Glsl.float1 g)
        (Glsl.float1 h)
        i


mat3ffff1ffff :
    Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3ffff1ffff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        (Glsl.float1 c)
        (Glsl.float1 d)
        e
        (Glsl.float1 f)
        (Glsl.float1 g)
        (Glsl.float1 h)
        (Glsl.float1 i)


mat3fffff1111 :
    Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3fffff1111 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        (Glsl.float1 c)
        (Glsl.float1 d)
        (Glsl.float1 e)
        f
        g
        h
        i


mat3fffff111f :
    Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3fffff111f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        (Glsl.float1 c)
        (Glsl.float1 d)
        (Glsl.float1 e)
        f
        g
        h
        (Glsl.float1 i)


mat3fffff11f1 :
    Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3fffff11f1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        (Glsl.float1 c)
        (Glsl.float1 d)
        (Glsl.float1 e)
        f
        g
        (Glsl.float1 h)
        i


mat3fffff11ff :
    Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3fffff11ff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        (Glsl.float1 c)
        (Glsl.float1 d)
        (Glsl.float1 e)
        f
        g
        (Glsl.float1 h)
        (Glsl.float1 i)


mat3fffff1f11 :
    Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3fffff1f11 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        (Glsl.float1 c)
        (Glsl.float1 d)
        (Glsl.float1 e)
        f
        (Glsl.float1 g)
        h
        i


mat3fffff1f1f :
    Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3fffff1f1f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        (Glsl.float1 c)
        (Glsl.float1 d)
        (Glsl.float1 e)
        f
        (Glsl.float1 g)
        h
        (Glsl.float1 i)


mat3fffff1ff1 :
    Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3fffff1ff1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        (Glsl.float1 c)
        (Glsl.float1 d)
        (Glsl.float1 e)
        f
        (Glsl.float1 g)
        (Glsl.float1 h)
        i


mat3fffff1fff :
    Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3fffff1fff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        (Glsl.float1 c)
        (Glsl.float1 d)
        (Glsl.float1 e)
        f
        (Glsl.float1 g)
        (Glsl.float1 h)
        (Glsl.float1 i)


mat3ffffff111 :
    Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3ffffff111 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        (Glsl.float1 c)
        (Glsl.float1 d)
        (Glsl.float1 e)
        (Glsl.float1 f)
        g
        h
        i


mat3ffffff11f :
    Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3ffffff11f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        (Glsl.float1 c)
        (Glsl.float1 d)
        (Glsl.float1 e)
        (Glsl.float1 f)
        g
        h
        (Glsl.float1 i)


mat3ffffff1f1 :
    Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3ffffff1f1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        (Glsl.float1 c)
        (Glsl.float1 d)
        (Glsl.float1 e)
        (Glsl.float1 f)
        g
        (Glsl.float1 h)
        i


mat3ffffff1ff :
    Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3ffffff1ff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        (Glsl.float1 c)
        (Glsl.float1 d)
        (Glsl.float1 e)
        (Glsl.float1 f)
        g
        (Glsl.float1 h)
        (Glsl.float1 i)


mat3fffffff11 :
    Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3fffffff11 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        (Glsl.float1 c)
        (Glsl.float1 d)
        (Glsl.float1 e)
        (Glsl.float1 f)
        (Glsl.float1 g)
        h
        i


mat3fffffff1f :
    Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3fffffff1f a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        (Glsl.float1 c)
        (Glsl.float1 d)
        (Glsl.float1 e)
        (Glsl.float1 f)
        (Glsl.float1 g)
        h
        (Glsl.float1 i)


mat3ffffffff1 :
    Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Mat3
mat3ffffffff1 a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        (Glsl.float1 c)
        (Glsl.float1 d)
        (Glsl.float1 e)
        (Glsl.float1 f)
        (Glsl.float1 g)
        (Glsl.float1 h)
        i


mat3fffffffff :
    Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Mat3
mat3fffffffff a b c d e f g h i =
    Glsl.unsafeCall9
        "mat3"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        (Glsl.float1 c)
        (Glsl.float1 d)
        (Glsl.float1 e)
        (Glsl.float1 f)
        (Glsl.float1 g)
        (Glsl.float1 h)
        (Glsl.float1 i)


mat44444 :
    Glsl.Expression Glsl.Vec4
    -> Glsl.Expression Glsl.Vec4
    -> Glsl.Expression Glsl.Vec4
    -> Glsl.Expression Glsl.Vec4
    -> Glsl.Expression Glsl.Mat4
mat44444 a b c d =
    Glsl.unsafeCall4 "mat4" [] a b c d


max11 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
max11 a b =
    Glsl.unsafeCall2 "max" [] a b


max1f : Glsl.Expression Glsl.Float_ -> Float -> Glsl.Expression Glsl.Float_
max1f a b =
    Glsl.unsafeCall2 "max" [] a (Glsl.float1 b)


max21 :
    Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec2
max21 a b =
    Glsl.unsafeCall2 "max" [] a b


max22 :
    Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
max22 a b =
    Glsl.unsafeCall2 "max" [] a b


max2f : Glsl.Expression Glsl.Vec2 -> Float -> Glsl.Expression Glsl.Vec2
max2f a b =
    Glsl.unsafeCall2 "max" [] a (Glsl.float1 b)


max31 :
    Glsl.Expression Glsl.Vec3
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec3
max31 a b =
    Glsl.unsafeCall2 "max" [] a b


max33 :
    Glsl.Expression Glsl.Vec3
    -> Glsl.Expression Glsl.Vec3
    -> Glsl.Expression Glsl.Vec3
max33 a b =
    Glsl.unsafeCall2 "max" [] a b


max3f : Glsl.Expression Glsl.Vec3 -> Float -> Glsl.Expression Glsl.Vec3
max3f a b =
    Glsl.unsafeCall2 "max" [] a (Glsl.float1 b)


max41 :
    Glsl.Expression Glsl.Vec4
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
max41 a b =
    Glsl.unsafeCall2 "max" [] a b


max44 :
    Glsl.Expression Glsl.Vec4
    -> Glsl.Expression Glsl.Vec4
    -> Glsl.Expression Glsl.Vec4
max44 a b =
    Glsl.unsafeCall2 "max" [] a b


max4f : Glsl.Expression Glsl.Vec4 -> Float -> Glsl.Expression Glsl.Vec4
max4f a b =
    Glsl.unsafeCall2 "max" [] a (Glsl.float1 b)


maxd1d1 :
    Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.Double
maxd1d1 a b =
    Glsl.unsafeCall2 "max" [] a b


maxd1wd1 : Glsl.Expression Glsl.Double -> Float -> Glsl.Expression Glsl.Double
maxd1wd1 a b =
    Glsl.unsafeCall2 "max" [] a (Glsl.double1 b)


maxd2d1 :
    Glsl.Expression Glsl.DVec2
    -> Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.DVec2
maxd2d1 a b =
    Glsl.unsafeCall2 "max" [] a b


maxd2d2 :
    Glsl.Expression Glsl.DVec2
    -> Glsl.Expression Glsl.DVec2
    -> Glsl.Expression Glsl.DVec2
maxd2d2 a b =
    Glsl.unsafeCall2 "max" [] a b


maxd2wd1 : Glsl.Expression Glsl.DVec2 -> Float -> Glsl.Expression Glsl.DVec2
maxd2wd1 a b =
    Glsl.unsafeCall2 "max" [] a (Glsl.double1 b)


maxd3d1 :
    Glsl.Expression Glsl.DVec3
    -> Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.DVec3
maxd3d1 a b =
    Glsl.unsafeCall2 "max" [] a b


maxd3d3 :
    Glsl.Expression Glsl.DVec3
    -> Glsl.Expression Glsl.DVec3
    -> Glsl.Expression Glsl.DVec3
maxd3d3 a b =
    Glsl.unsafeCall2 "max" [] a b


maxd3wd1 : Glsl.Expression Glsl.DVec3 -> Float -> Glsl.Expression Glsl.DVec3
maxd3wd1 a b =
    Glsl.unsafeCall2 "max" [] a (Glsl.double1 b)


maxd4d1 :
    Glsl.Expression Glsl.DVec4
    -> Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.DVec4
maxd4d1 a b =
    Glsl.unsafeCall2 "max" [] a b


maxd4d4 :
    Glsl.Expression Glsl.DVec4
    -> Glsl.Expression Glsl.DVec4
    -> Glsl.Expression Glsl.DVec4
maxd4d4 a b =
    Glsl.unsafeCall2 "max" [] a b


maxd4wd1 : Glsl.Expression Glsl.DVec4 -> Float -> Glsl.Expression Glsl.DVec4
maxd4wd1 a b =
    Glsl.unsafeCall2 "max" [] a (Glsl.double1 b)


maxf1 : Float -> Glsl.Expression Glsl.Float_ -> Glsl.Expression Glsl.Float_
maxf1 a b =
    Glsl.unsafeCall2 "max" [] (Glsl.float1 a) b


maxff : Float -> Float -> Glsl.Expression Glsl.Float_
maxff a b =
    Glsl.unsafeCall2 "max" [] (Glsl.float1 a) (Glsl.float1 b)


maxi1i1 :
    Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
maxi1i1 a b =
    Glsl.unsafeCall2 "max" [] a b


maxi1wi1 : Glsl.Expression Glsl.Int_ -> Int -> Glsl.Expression Glsl.Int_
maxi1wi1 a b =
    Glsl.unsafeCall2 "max" [] a (Glsl.int1 b)


maxi2i1 :
    Glsl.Expression Glsl.IVec2
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.IVec2
maxi2i1 a b =
    Glsl.unsafeCall2 "max" [] a b


maxi2i2 :
    Glsl.Expression Glsl.IVec2
    -> Glsl.Expression Glsl.IVec2
    -> Glsl.Expression Glsl.IVec2
maxi2i2 a b =
    Glsl.unsafeCall2 "max" [] a b


maxi2wi1 : Glsl.Expression Glsl.IVec2 -> Int -> Glsl.Expression Glsl.IVec2
maxi2wi1 a b =
    Glsl.unsafeCall2 "max" [] a (Glsl.int1 b)


maxi3i1 :
    Glsl.Expression Glsl.IVec3
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.IVec3
maxi3i1 a b =
    Glsl.unsafeCall2 "max" [] a b


maxi3i3 :
    Glsl.Expression Glsl.IVec3
    -> Glsl.Expression Glsl.IVec3
    -> Glsl.Expression Glsl.IVec3
maxi3i3 a b =
    Glsl.unsafeCall2 "max" [] a b


maxi3wi1 : Glsl.Expression Glsl.IVec3 -> Int -> Glsl.Expression Glsl.IVec3
maxi3wi1 a b =
    Glsl.unsafeCall2 "max" [] a (Glsl.int1 b)


maxi4i1 :
    Glsl.Expression Glsl.IVec4
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.IVec4
maxi4i1 a b =
    Glsl.unsafeCall2 "max" [] a b


maxi4i4 :
    Glsl.Expression Glsl.IVec4
    -> Glsl.Expression Glsl.IVec4
    -> Glsl.Expression Glsl.IVec4
maxi4i4 a b =
    Glsl.unsafeCall2 "max" [] a b


maxi4wi1 : Glsl.Expression Glsl.IVec4 -> Int -> Glsl.Expression Glsl.IVec4
maxi4wi1 a b =
    Glsl.unsafeCall2 "max" [] a (Glsl.int1 b)


maxu1u1 :
    Glsl.Expression Glsl.Uint
    -> Glsl.Expression Glsl.Uint
    -> Glsl.Expression Glsl.Uint
maxu1u1 a b =
    Glsl.unsafeCall2 "max" [] a b


maxu1wu1 : Glsl.Expression Glsl.Uint -> Int -> Glsl.Expression Glsl.Uint
maxu1wu1 a b =
    Glsl.unsafeCall2 "max" [] a (Glsl.uint1 b)


maxu2u1 :
    Glsl.Expression Glsl.UVec2
    -> Glsl.Expression Glsl.Uint
    -> Glsl.Expression Glsl.UVec2
maxu2u1 a b =
    Glsl.unsafeCall2 "max" [] a b


maxu2u2 :
    Glsl.Expression Glsl.UVec2
    -> Glsl.Expression Glsl.UVec2
    -> Glsl.Expression Glsl.UVec2
maxu2u2 a b =
    Glsl.unsafeCall2 "max" [] a b


maxu2wu1 : Glsl.Expression Glsl.UVec2 -> Int -> Glsl.Expression Glsl.UVec2
maxu2wu1 a b =
    Glsl.unsafeCall2 "max" [] a (Glsl.uint1 b)


maxu3u1 :
    Glsl.Expression Glsl.UVec3
    -> Glsl.Expression Glsl.Uint
    -> Glsl.Expression Glsl.UVec3
maxu3u1 a b =
    Glsl.unsafeCall2 "max" [] a b


maxu3u3 :
    Glsl.Expression Glsl.UVec3
    -> Glsl.Expression Glsl.UVec3
    -> Glsl.Expression Glsl.UVec3
maxu3u3 a b =
    Glsl.unsafeCall2 "max" [] a b


maxu3wu1 : Glsl.Expression Glsl.UVec3 -> Int -> Glsl.Expression Glsl.UVec3
maxu3wu1 a b =
    Glsl.unsafeCall2 "max" [] a (Glsl.uint1 b)


maxu4u1 :
    Glsl.Expression Glsl.UVec4
    -> Glsl.Expression Glsl.Uint
    -> Glsl.Expression Glsl.UVec4
maxu4u1 a b =
    Glsl.unsafeCall2 "max" [] a b


maxu4u4 :
    Glsl.Expression Glsl.UVec4
    -> Glsl.Expression Glsl.UVec4
    -> Glsl.Expression Glsl.UVec4
maxu4u4 a b =
    Glsl.unsafeCall2 "max" [] a b


maxu4wu1 : Glsl.Expression Glsl.UVec4 -> Int -> Glsl.Expression Glsl.UVec4
maxu4wu1 a b =
    Glsl.unsafeCall2 "max" [] a (Glsl.uint1 b)


maxwd1d1 : Float -> Glsl.Expression Glsl.Double -> Glsl.Expression Glsl.Double
maxwd1d1 a b =
    Glsl.unsafeCall2 "max" [] (Glsl.double1 a) b


maxwd1wd1 : Float -> Float -> Glsl.Expression Glsl.Double
maxwd1wd1 a b =
    Glsl.unsafeCall2 "max" [] (Glsl.double1 a) (Glsl.double1 b)


maxwi1i1 : Int -> Glsl.Expression Glsl.Int_ -> Glsl.Expression Glsl.Int_
maxwi1i1 a b =
    Glsl.unsafeCall2 "max" [] (Glsl.int1 a) b


maxwi1wi1 : Int -> Int -> Glsl.Expression Glsl.Int_
maxwi1wi1 a b =
    Glsl.unsafeCall2 "max" [] (Glsl.int1 a) (Glsl.int1 b)


maxwu1u1 : Int -> Glsl.Expression Glsl.Uint -> Glsl.Expression Glsl.Uint
maxwu1u1 a b =
    Glsl.unsafeCall2 "max" [] (Glsl.uint1 a) b


maxwu1wu1 : Int -> Int -> Glsl.Expression Glsl.Uint
maxwu1wu1 a b =
    Glsl.unsafeCall2 "max" [] (Glsl.uint1 a) (Glsl.uint1 b)


min11 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
min11 a b =
    Glsl.unsafeCall2 "min" [] a b


min1f : Glsl.Expression Glsl.Float_ -> Float -> Glsl.Expression Glsl.Float_
min1f a b =
    Glsl.unsafeCall2 "min" [] a (Glsl.float1 b)


min21 :
    Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec2
min21 a b =
    Glsl.unsafeCall2 "min" [] a b


min22 :
    Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
min22 a b =
    Glsl.unsafeCall2 "min" [] a b


min2f : Glsl.Expression Glsl.Vec2 -> Float -> Glsl.Expression Glsl.Vec2
min2f a b =
    Glsl.unsafeCall2 "min" [] a (Glsl.float1 b)


min31 :
    Glsl.Expression Glsl.Vec3
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec3
min31 a b =
    Glsl.unsafeCall2 "min" [] a b


min33 :
    Glsl.Expression Glsl.Vec3
    -> Glsl.Expression Glsl.Vec3
    -> Glsl.Expression Glsl.Vec3
min33 a b =
    Glsl.unsafeCall2 "min" [] a b


min3f : Glsl.Expression Glsl.Vec3 -> Float -> Glsl.Expression Glsl.Vec3
min3f a b =
    Glsl.unsafeCall2 "min" [] a (Glsl.float1 b)


min41 :
    Glsl.Expression Glsl.Vec4
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
min41 a b =
    Glsl.unsafeCall2 "min" [] a b


min44 :
    Glsl.Expression Glsl.Vec4
    -> Glsl.Expression Glsl.Vec4
    -> Glsl.Expression Glsl.Vec4
min44 a b =
    Glsl.unsafeCall2 "min" [] a b


min4f : Glsl.Expression Glsl.Vec4 -> Float -> Glsl.Expression Glsl.Vec4
min4f a b =
    Glsl.unsafeCall2 "min" [] a (Glsl.float1 b)


mind1d1 :
    Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.Double
mind1d1 a b =
    Glsl.unsafeCall2 "min" [] a b


mind1wd1 : Glsl.Expression Glsl.Double -> Float -> Glsl.Expression Glsl.Double
mind1wd1 a b =
    Glsl.unsafeCall2 "min" [] a (Glsl.double1 b)


mind2d1 :
    Glsl.Expression Glsl.DVec2
    -> Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.DVec2
mind2d1 a b =
    Glsl.unsafeCall2 "min" [] a b


mind2d2 :
    Glsl.Expression Glsl.DVec2
    -> Glsl.Expression Glsl.DVec2
    -> Glsl.Expression Glsl.DVec2
mind2d2 a b =
    Glsl.unsafeCall2 "min" [] a b


mind2wd1 : Glsl.Expression Glsl.DVec2 -> Float -> Glsl.Expression Glsl.DVec2
mind2wd1 a b =
    Glsl.unsafeCall2 "min" [] a (Glsl.double1 b)


mind3d1 :
    Glsl.Expression Glsl.DVec3
    -> Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.DVec3
mind3d1 a b =
    Glsl.unsafeCall2 "min" [] a b


mind3d3 :
    Glsl.Expression Glsl.DVec3
    -> Glsl.Expression Glsl.DVec3
    -> Glsl.Expression Glsl.DVec3
mind3d3 a b =
    Glsl.unsafeCall2 "min" [] a b


mind3wd1 : Glsl.Expression Glsl.DVec3 -> Float -> Glsl.Expression Glsl.DVec3
mind3wd1 a b =
    Glsl.unsafeCall2 "min" [] a (Glsl.double1 b)


mind4d1 :
    Glsl.Expression Glsl.DVec4
    -> Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.DVec4
mind4d1 a b =
    Glsl.unsafeCall2 "min" [] a b


mind4d4 :
    Glsl.Expression Glsl.DVec4
    -> Glsl.Expression Glsl.DVec4
    -> Glsl.Expression Glsl.DVec4
mind4d4 a b =
    Glsl.unsafeCall2 "min" [] a b


mind4wd1 : Glsl.Expression Glsl.DVec4 -> Float -> Glsl.Expression Glsl.DVec4
mind4wd1 a b =
    Glsl.unsafeCall2 "min" [] a (Glsl.double1 b)


minf1 : Float -> Glsl.Expression Glsl.Float_ -> Glsl.Expression Glsl.Float_
minf1 a b =
    Glsl.unsafeCall2 "min" [] (Glsl.float1 a) b


minff : Float -> Float -> Glsl.Expression Glsl.Float_
minff a b =
    Glsl.unsafeCall2 "min" [] (Glsl.float1 a) (Glsl.float1 b)


mini1i1 :
    Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
mini1i1 a b =
    Glsl.unsafeCall2 "min" [] a b


mini1wi1 : Glsl.Expression Glsl.Int_ -> Int -> Glsl.Expression Glsl.Int_
mini1wi1 a b =
    Glsl.unsafeCall2 "min" [] a (Glsl.int1 b)


mini2i1 :
    Glsl.Expression Glsl.IVec2
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.IVec2
mini2i1 a b =
    Glsl.unsafeCall2 "min" [] a b


mini2i2 :
    Glsl.Expression Glsl.IVec2
    -> Glsl.Expression Glsl.IVec2
    -> Glsl.Expression Glsl.IVec2
mini2i2 a b =
    Glsl.unsafeCall2 "min" [] a b


mini2wi1 : Glsl.Expression Glsl.IVec2 -> Int -> Glsl.Expression Glsl.IVec2
mini2wi1 a b =
    Glsl.unsafeCall2 "min" [] a (Glsl.int1 b)


mini3i1 :
    Glsl.Expression Glsl.IVec3
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.IVec3
mini3i1 a b =
    Glsl.unsafeCall2 "min" [] a b


mini3i3 :
    Glsl.Expression Glsl.IVec3
    -> Glsl.Expression Glsl.IVec3
    -> Glsl.Expression Glsl.IVec3
mini3i3 a b =
    Glsl.unsafeCall2 "min" [] a b


mini3wi1 : Glsl.Expression Glsl.IVec3 -> Int -> Glsl.Expression Glsl.IVec3
mini3wi1 a b =
    Glsl.unsafeCall2 "min" [] a (Glsl.int1 b)


mini4i1 :
    Glsl.Expression Glsl.IVec4
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.IVec4
mini4i1 a b =
    Glsl.unsafeCall2 "min" [] a b


mini4i4 :
    Glsl.Expression Glsl.IVec4
    -> Glsl.Expression Glsl.IVec4
    -> Glsl.Expression Glsl.IVec4
mini4i4 a b =
    Glsl.unsafeCall2 "min" [] a b


mini4wi1 : Glsl.Expression Glsl.IVec4 -> Int -> Glsl.Expression Glsl.IVec4
mini4wi1 a b =
    Glsl.unsafeCall2 "min" [] a (Glsl.int1 b)


minu1u1 :
    Glsl.Expression Glsl.Uint
    -> Glsl.Expression Glsl.Uint
    -> Glsl.Expression Glsl.Uint
minu1u1 a b =
    Glsl.unsafeCall2 "min" [] a b


minu1wu1 : Glsl.Expression Glsl.Uint -> Int -> Glsl.Expression Glsl.Uint
minu1wu1 a b =
    Glsl.unsafeCall2 "min" [] a (Glsl.uint1 b)


minu2u1 :
    Glsl.Expression Glsl.UVec2
    -> Glsl.Expression Glsl.Uint
    -> Glsl.Expression Glsl.UVec2
minu2u1 a b =
    Glsl.unsafeCall2 "min" [] a b


minu2u2 :
    Glsl.Expression Glsl.UVec2
    -> Glsl.Expression Glsl.UVec2
    -> Glsl.Expression Glsl.UVec2
minu2u2 a b =
    Glsl.unsafeCall2 "min" [] a b


minu2wu1 : Glsl.Expression Glsl.UVec2 -> Int -> Glsl.Expression Glsl.UVec2
minu2wu1 a b =
    Glsl.unsafeCall2 "min" [] a (Glsl.uint1 b)


minu3u1 :
    Glsl.Expression Glsl.UVec3
    -> Glsl.Expression Glsl.Uint
    -> Glsl.Expression Glsl.UVec3
minu3u1 a b =
    Glsl.unsafeCall2 "min" [] a b


minu3u3 :
    Glsl.Expression Glsl.UVec3
    -> Glsl.Expression Glsl.UVec3
    -> Glsl.Expression Glsl.UVec3
minu3u3 a b =
    Glsl.unsafeCall2 "min" [] a b


minu3wu1 : Glsl.Expression Glsl.UVec3 -> Int -> Glsl.Expression Glsl.UVec3
minu3wu1 a b =
    Glsl.unsafeCall2 "min" [] a (Glsl.uint1 b)


minu4u1 :
    Glsl.Expression Glsl.UVec4
    -> Glsl.Expression Glsl.Uint
    -> Glsl.Expression Glsl.UVec4
minu4u1 a b =
    Glsl.unsafeCall2 "min" [] a b


minu4u4 :
    Glsl.Expression Glsl.UVec4
    -> Glsl.Expression Glsl.UVec4
    -> Glsl.Expression Glsl.UVec4
minu4u4 a b =
    Glsl.unsafeCall2 "min" [] a b


minu4wu1 : Glsl.Expression Glsl.UVec4 -> Int -> Glsl.Expression Glsl.UVec4
minu4wu1 a b =
    Glsl.unsafeCall2 "min" [] a (Glsl.uint1 b)


minwd1d1 : Float -> Glsl.Expression Glsl.Double -> Glsl.Expression Glsl.Double
minwd1d1 a b =
    Glsl.unsafeCall2 "min" [] (Glsl.double1 a) b


minwd1wd1 : Float -> Float -> Glsl.Expression Glsl.Double
minwd1wd1 a b =
    Glsl.unsafeCall2 "min" [] (Glsl.double1 a) (Glsl.double1 b)


minwi1i1 : Int -> Glsl.Expression Glsl.Int_ -> Glsl.Expression Glsl.Int_
minwi1i1 a b =
    Glsl.unsafeCall2 "min" [] (Glsl.int1 a) b


minwi1wi1 : Int -> Int -> Glsl.Expression Glsl.Int_
minwi1wi1 a b =
    Glsl.unsafeCall2 "min" [] (Glsl.int1 a) (Glsl.int1 b)


minwu1u1 : Int -> Glsl.Expression Glsl.Uint -> Glsl.Expression Glsl.Uint
minwu1u1 a b =
    Glsl.unsafeCall2 "min" [] (Glsl.uint1 a) b


minwu1wu1 : Int -> Int -> Glsl.Expression Glsl.Uint
minwu1wu1 a b =
    Glsl.unsafeCall2 "min" [] (Glsl.uint1 a) (Glsl.uint1 b)


mix111 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
mix111 a b c =
    Glsl.unsafeCall3 "mix" [] a b c


mix11f :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
mix11f a b c =
    Glsl.unsafeCall3 "mix" [] a b (Glsl.float1 c)


mix1f1 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
mix1f1 a b c =
    Glsl.unsafeCall3 "mix" [] a (Glsl.float1 b) c


mix1ff : Glsl.Expression Glsl.Float_ -> Float -> Float -> Glsl.Expression Glsl.Float_
mix1ff a b c =
    Glsl.unsafeCall3 "mix" [] a (Glsl.float1 b) (Glsl.float1 c)


mix221 :
    Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec2
mix221 a b c =
    Glsl.unsafeCall3 "mix" [] a b c


mix222 :
    Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
mix222 a b c =
    Glsl.unsafeCall3 "mix" [] a b c


mix22f :
    Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
    -> Float
    -> Glsl.Expression Glsl.Vec2
mix22f a b c =
    Glsl.unsafeCall3 "mix" [] a b (Glsl.float1 c)


mix331 :
    Glsl.Expression Glsl.Vec3
    -> Glsl.Expression Glsl.Vec3
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec3
mix331 a b c =
    Glsl.unsafeCall3 "mix" [] a b c


mix333 :
    Glsl.Expression Glsl.Vec3
    -> Glsl.Expression Glsl.Vec3
    -> Glsl.Expression Glsl.Vec3
    -> Glsl.Expression Glsl.Vec3
mix333 a b c =
    Glsl.unsafeCall3 "mix" [] a b c


mix33f :
    Glsl.Expression Glsl.Vec3
    -> Glsl.Expression Glsl.Vec3
    -> Float
    -> Glsl.Expression Glsl.Vec3
mix33f a b c =
    Glsl.unsafeCall3 "mix" [] a b (Glsl.float1 c)


mix441 :
    Glsl.Expression Glsl.Vec4
    -> Glsl.Expression Glsl.Vec4
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
mix441 a b c =
    Glsl.unsafeCall3 "mix" [] a b c


mix444 :
    Glsl.Expression Glsl.Vec4
    -> Glsl.Expression Glsl.Vec4
    -> Glsl.Expression Glsl.Vec4
    -> Glsl.Expression Glsl.Vec4
mix444 a b c =
    Glsl.unsafeCall3 "mix" [] a b c


mix44f :
    Glsl.Expression Glsl.Vec4
    -> Glsl.Expression Glsl.Vec4
    -> Float
    -> Glsl.Expression Glsl.Vec4
mix44f a b c =
    Glsl.unsafeCall3 "mix" [] a b (Glsl.float1 c)


mixd1d1d1 :
    Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.Double
mixd1d1d1 a b c =
    Glsl.unsafeCall3 "mix" [] a b c


mixd1d1wd1 :
    Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.Double
    -> Float
    -> Glsl.Expression Glsl.Double
mixd1d1wd1 a b c =
    Glsl.unsafeCall3 "mix" [] a b (Glsl.double1 c)


mixd1wd1d1 :
    Glsl.Expression Glsl.Double
    -> Float
    -> Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.Double
mixd1wd1d1 a b c =
    Glsl.unsafeCall3 "mix" [] a (Glsl.double1 b) c


mixd1wd1wd1 : Glsl.Expression Glsl.Double -> Float -> Float -> Glsl.Expression Glsl.Double
mixd1wd1wd1 a b c =
    Glsl.unsafeCall3 "mix" [] a (Glsl.double1 b) (Glsl.double1 c)


mixd2d2d1 :
    Glsl.Expression Glsl.DVec2
    -> Glsl.Expression Glsl.DVec2
    -> Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.DVec2
mixd2d2d1 a b c =
    Glsl.unsafeCall3 "mix" [] a b c


mixd2d2d2 :
    Glsl.Expression Glsl.DVec2
    -> Glsl.Expression Glsl.DVec2
    -> Glsl.Expression Glsl.DVec2
    -> Glsl.Expression Glsl.DVec2
mixd2d2d2 a b c =
    Glsl.unsafeCall3 "mix" [] a b c


mixd2d2wd1 :
    Glsl.Expression Glsl.DVec2
    -> Glsl.Expression Glsl.DVec2
    -> Float
    -> Glsl.Expression Glsl.DVec2
mixd2d2wd1 a b c =
    Glsl.unsafeCall3 "mix" [] a b (Glsl.double1 c)


mixd3d3d1 :
    Glsl.Expression Glsl.DVec3
    -> Glsl.Expression Glsl.DVec3
    -> Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.DVec3
mixd3d3d1 a b c =
    Glsl.unsafeCall3 "mix" [] a b c


mixd3d3d3 :
    Glsl.Expression Glsl.DVec3
    -> Glsl.Expression Glsl.DVec3
    -> Glsl.Expression Glsl.DVec3
    -> Glsl.Expression Glsl.DVec3
mixd3d3d3 a b c =
    Glsl.unsafeCall3 "mix" [] a b c


mixd3d3wd1 :
    Glsl.Expression Glsl.DVec3
    -> Glsl.Expression Glsl.DVec3
    -> Float
    -> Glsl.Expression Glsl.DVec3
mixd3d3wd1 a b c =
    Glsl.unsafeCall3 "mix" [] a b (Glsl.double1 c)


mixd4d4d1 :
    Glsl.Expression Glsl.DVec4
    -> Glsl.Expression Glsl.DVec4
    -> Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.DVec4
mixd4d4d1 a b c =
    Glsl.unsafeCall3 "mix" [] a b c


mixd4d4d4 :
    Glsl.Expression Glsl.DVec4
    -> Glsl.Expression Glsl.DVec4
    -> Glsl.Expression Glsl.DVec4
    -> Glsl.Expression Glsl.DVec4
mixd4d4d4 a b c =
    Glsl.unsafeCall3 "mix" [] a b c


mixd4d4wd1 :
    Glsl.Expression Glsl.DVec4
    -> Glsl.Expression Glsl.DVec4
    -> Float
    -> Glsl.Expression Glsl.DVec4
mixd4d4wd1 a b c =
    Glsl.unsafeCall3 "mix" [] a b (Glsl.double1 c)


mixf11 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
mixf11 a b c =
    Glsl.unsafeCall3 "mix" [] (Glsl.float1 a) b c


mixf1f : Float -> Glsl.Expression Glsl.Float_ -> Float -> Glsl.Expression Glsl.Float_
mixf1f a b c =
    Glsl.unsafeCall3 "mix" [] (Glsl.float1 a) b (Glsl.float1 c)


mixff1 : Float -> Float -> Glsl.Expression Glsl.Float_ -> Glsl.Expression Glsl.Float_
mixff1 a b c =
    Glsl.unsafeCall3 "mix" [] (Glsl.float1 a) (Glsl.float1 b) c


mixfff : Float -> Float -> Float -> Glsl.Expression Glsl.Float_
mixfff a b c =
    Glsl.unsafeCall3 "mix" [] (Glsl.float1 a) (Glsl.float1 b) (Glsl.float1 c)


mixwd1d1d1 :
    Float
    -> Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.Double
mixwd1d1d1 a b c =
    Glsl.unsafeCall3 "mix" [] (Glsl.double1 a) b c


mixwd1d1wd1 : Float -> Glsl.Expression Glsl.Double -> Float -> Glsl.Expression Glsl.Double
mixwd1d1wd1 a b c =
    Glsl.unsafeCall3 "mix" [] (Glsl.double1 a) b (Glsl.double1 c)


mixwd1wd1d1 : Float -> Float -> Glsl.Expression Glsl.Double -> Glsl.Expression Glsl.Double
mixwd1wd1d1 a b c =
    Glsl.unsafeCall3 "mix" [] (Glsl.double1 a) (Glsl.double1 b) c


mixwd1wd1wd1 : Float -> Float -> Float -> Glsl.Expression Glsl.Double
mixwd1wd1wd1 a b c =
    Glsl.unsafeCall3 "mix" [] (Glsl.double1 a) (Glsl.double1 b) (Glsl.double1 c)


mix :
    Glsl.Expression (Glsl.Vec t a)
    -> Glsl.Expression (Glsl.Vec t a)
    -> Glsl.Expression (Glsl.Vec t a)
    -> Glsl.Expression (Glsl.Vec t a)
mix a b c =
    Glsl.unsafeCall3 "mix" [] a b c


mod11 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
mod11 a b =
    Glsl.unsafeCall2 "mod" [] a b


mod1f : Glsl.Expression Glsl.Float_ -> Float -> Glsl.Expression Glsl.Float_
mod1f a b =
    Glsl.unsafeCall2 "mod" [] a (Glsl.float1 b)


mod21 :
    Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec2
mod21 a b =
    Glsl.unsafeCall2 "mod" [] a b


mod22 :
    Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
mod22 a b =
    Glsl.unsafeCall2 "mod" [] a b


mod2f : Glsl.Expression Glsl.Vec2 -> Float -> Glsl.Expression Glsl.Vec2
mod2f a b =
    Glsl.unsafeCall2 "mod" [] a (Glsl.float1 b)


mod31 :
    Glsl.Expression Glsl.Vec3
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec3
mod31 a b =
    Glsl.unsafeCall2 "mod" [] a b


mod33 :
    Glsl.Expression Glsl.Vec3
    -> Glsl.Expression Glsl.Vec3
    -> Glsl.Expression Glsl.Vec3
mod33 a b =
    Glsl.unsafeCall2 "mod" [] a b


mod3f : Glsl.Expression Glsl.Vec3 -> Float -> Glsl.Expression Glsl.Vec3
mod3f a b =
    Glsl.unsafeCall2 "mod" [] a (Glsl.float1 b)


mod41 :
    Glsl.Expression Glsl.Vec4
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
mod41 a b =
    Glsl.unsafeCall2 "mod" [] a b


mod44 :
    Glsl.Expression Glsl.Vec4
    -> Glsl.Expression Glsl.Vec4
    -> Glsl.Expression Glsl.Vec4
mod44 a b =
    Glsl.unsafeCall2 "mod" [] a b


mod4f : Glsl.Expression Glsl.Vec4 -> Float -> Glsl.Expression Glsl.Vec4
mod4f a b =
    Glsl.unsafeCall2 "mod" [] a (Glsl.float1 b)


modd1d1 :
    Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.Double
modd1d1 a b =
    Glsl.unsafeCall2 "mod" [] a b


modd1wd1 : Glsl.Expression Glsl.Double -> Float -> Glsl.Expression Glsl.Double
modd1wd1 a b =
    Glsl.unsafeCall2 "mod" [] a (Glsl.double1 b)


modd2d1 :
    Glsl.Expression Glsl.DVec2
    -> Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.DVec2
modd2d1 a b =
    Glsl.unsafeCall2 "mod" [] a b


modd2d2 :
    Glsl.Expression Glsl.DVec2
    -> Glsl.Expression Glsl.DVec2
    -> Glsl.Expression Glsl.DVec2
modd2d2 a b =
    Glsl.unsafeCall2 "mod" [] a b


modd2wd1 : Glsl.Expression Glsl.DVec2 -> Float -> Glsl.Expression Glsl.DVec2
modd2wd1 a b =
    Glsl.unsafeCall2 "mod" [] a (Glsl.double1 b)


modd3d1 :
    Glsl.Expression Glsl.DVec3
    -> Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.DVec3
modd3d1 a b =
    Glsl.unsafeCall2 "mod" [] a b


modd3d3 :
    Glsl.Expression Glsl.DVec3
    -> Glsl.Expression Glsl.DVec3
    -> Glsl.Expression Glsl.DVec3
modd3d3 a b =
    Glsl.unsafeCall2 "mod" [] a b


modd3wd1 : Glsl.Expression Glsl.DVec3 -> Float -> Glsl.Expression Glsl.DVec3
modd3wd1 a b =
    Glsl.unsafeCall2 "mod" [] a (Glsl.double1 b)


modd4d1 :
    Glsl.Expression Glsl.DVec4
    -> Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.DVec4
modd4d1 a b =
    Glsl.unsafeCall2 "mod" [] a b


modd4d4 :
    Glsl.Expression Glsl.DVec4
    -> Glsl.Expression Glsl.DVec4
    -> Glsl.Expression Glsl.DVec4
modd4d4 a b =
    Glsl.unsafeCall2 "mod" [] a b


modd4wd1 : Glsl.Expression Glsl.DVec4 -> Float -> Glsl.Expression Glsl.DVec4
modd4wd1 a b =
    Glsl.unsafeCall2 "mod" [] a (Glsl.double1 b)


modf1 : Float -> Glsl.Expression Glsl.Float_ -> Glsl.Expression Glsl.Float_
modf1 a b =
    Glsl.unsafeCall2 "mod" [] (Glsl.float1 a) b


modff : Float -> Float -> Glsl.Expression Glsl.Float_
modff a b =
    Glsl.unsafeCall2 "mod" [] (Glsl.float1 a) (Glsl.float1 b)


modwd1d1 : Float -> Glsl.Expression Glsl.Double -> Glsl.Expression Glsl.Double
modwd1d1 a b =
    Glsl.unsafeCall2 "mod" [] (Glsl.double1 a) b


modwd1wd1 : Float -> Float -> Glsl.Expression Glsl.Double
modwd1wd1 a b =
    Glsl.unsafeCall2 "mod" [] (Glsl.double1 a) (Glsl.double1 b)


mod :
    Glsl.Expression (Glsl.Vec t a)
    -> Glsl.Expression (Glsl.Vec t a)
    -> Glsl.Expression (Glsl.Vec t a)
mod a b =
    Glsl.unsafeCall2 "mod" [] a b


modf1o1 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression (Glsl.Out Glsl.Float_)
    -> Glsl.Expression Glsl.Float_
modf1o1 a b =
    Glsl.unsafeCall2 "modf" [] a b


modf1of : Glsl.Expression Glsl.Float_ -> Float -> Glsl.Expression Glsl.Float_
modf1of a b =
    Glsl.unsafeCall2 "modf" [] a (Glsl.float1 b)


modf2o2 :
    Glsl.Expression Glsl.Vec2
    -> Glsl.Expression (Glsl.Out Glsl.Vec2)
    -> Glsl.Expression Glsl.Vec2
modf2o2 a b =
    Glsl.unsafeCall2 "modf" [] a b


modf3o3 :
    Glsl.Expression Glsl.Vec3
    -> Glsl.Expression (Glsl.Out Glsl.Vec3)
    -> Glsl.Expression Glsl.Vec3
modf3o3 a b =
    Glsl.unsafeCall2 "modf" [] a b


modf4o4 :
    Glsl.Expression Glsl.Vec4
    -> Glsl.Expression (Glsl.Out Glsl.Vec4)
    -> Glsl.Expression Glsl.Vec4
modf4o4 a b =
    Glsl.unsafeCall2 "modf" [] a b


modfd1od1 :
    Glsl.Expression Glsl.Double
    -> Glsl.Expression (Glsl.Out Glsl.Double)
    -> Glsl.Expression Glsl.Double
modfd1od1 a b =
    Glsl.unsafeCall2 "modf" [] a b


modfd1owd1 : Glsl.Expression Glsl.Double -> Float -> Glsl.Expression Glsl.Double
modfd1owd1 a b =
    Glsl.unsafeCall2 "modf" [] a (Glsl.double1 b)


modfd2od2 :
    Glsl.Expression Glsl.DVec2
    -> Glsl.Expression (Glsl.Out Glsl.DVec2)
    -> Glsl.Expression Glsl.DVec2
modfd2od2 a b =
    Glsl.unsafeCall2 "modf" [] a b


modfd3od3 :
    Glsl.Expression Glsl.DVec3
    -> Glsl.Expression (Glsl.Out Glsl.DVec3)
    -> Glsl.Expression Glsl.DVec3
modfd3od3 a b =
    Glsl.unsafeCall2 "modf" [] a b


modfd4od4 :
    Glsl.Expression Glsl.DVec4
    -> Glsl.Expression (Glsl.Out Glsl.DVec4)
    -> Glsl.Expression Glsl.DVec4
modfd4od4 a b =
    Glsl.unsafeCall2 "modf" [] a b


modffo1 :
    Float
    -> Glsl.Expression (Glsl.Out Glsl.Float_)
    -> Glsl.Expression Glsl.Float_
modffo1 a b =
    Glsl.unsafeCall2 "modf" [] (Glsl.float1 a) b


modffof : Float -> Float -> Glsl.Expression Glsl.Float_
modffof a b =
    Glsl.unsafeCall2 "modf" [] (Glsl.float1 a) (Glsl.float1 b)


modfwd1od1 :
    Float
    -> Glsl.Expression (Glsl.Out Glsl.Double)
    -> Glsl.Expression Glsl.Double
modfwd1od1 a b =
    Glsl.unsafeCall2 "modf" [] (Glsl.double1 a) b


modfwd1owd1 : Float -> Float -> Glsl.Expression Glsl.Double
modfwd1owd1 a b =
    Glsl.unsafeCall2 "modf" [] (Glsl.double1 a) (Glsl.double1 b)


modf :
    Glsl.Expression (Glsl.Vec t a)
    -> Glsl.Expression (Glsl.Out (Glsl.Vec t a))
    -> Glsl.Expression (Glsl.Vec t a)
modf a b =
    Glsl.unsafeCall2 "modf" [] a b


normalize1 : Glsl.Expression Glsl.Float_ -> Glsl.Expression Glsl.Float_
normalize1 a =
    Glsl.unsafeCall1 "normalize" [] a


normalize2 : Glsl.Expression Glsl.Vec2 -> Glsl.Expression Glsl.Vec2
normalize2 a =
    Glsl.unsafeCall1 "normalize" [] a


normalize3 : Glsl.Expression Glsl.Vec3 -> Glsl.Expression Glsl.Vec3
normalize3 a =
    Glsl.unsafeCall1 "normalize" [] a


normalize4 : Glsl.Expression Glsl.Vec4 -> Glsl.Expression Glsl.Vec4
normalize4 a =
    Glsl.unsafeCall1 "normalize" [] a


normalized1 : Glsl.Expression Glsl.Double -> Glsl.Expression Glsl.Double
normalized1 a =
    Glsl.unsafeCall1 "normalize" [] a


normalized2 : Glsl.Expression Glsl.DVec2 -> Glsl.Expression Glsl.DVec2
normalized2 a =
    Glsl.unsafeCall1 "normalize" [] a


normalized3 : Glsl.Expression Glsl.DVec3 -> Glsl.Expression Glsl.DVec3
normalized3 a =
    Glsl.unsafeCall1 "normalize" [] a


normalized4 : Glsl.Expression Glsl.DVec4 -> Glsl.Expression Glsl.DVec4
normalized4 a =
    Glsl.unsafeCall1 "normalize" [] a


normalizef : Float -> Glsl.Expression Glsl.Float_
normalizef a =
    Glsl.unsafeCall1 "normalize" [] (Glsl.float1 a)


normalizewd1 : Float -> Glsl.Expression Glsl.Double
normalizewd1 a =
    Glsl.unsafeCall1 "normalize" [] (Glsl.double1 a)


normalize : Glsl.Expression (Glsl.Vec t a) -> Glsl.Expression (Glsl.Vec t a)
normalize a =
    Glsl.unsafeCall1 "normalize" [] a


reflect11 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
reflect11 a b =
    Glsl.unsafeCall2 "reflect" [] a b


reflect1f : Glsl.Expression Glsl.Float_ -> Float -> Glsl.Expression Glsl.Float_
reflect1f a b =
    Glsl.unsafeCall2 "reflect" [] a (Glsl.float1 b)


reflect22 :
    Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
reflect22 a b =
    Glsl.unsafeCall2 "reflect" [] a b


reflect33 :
    Glsl.Expression Glsl.Vec3
    -> Glsl.Expression Glsl.Vec3
    -> Glsl.Expression Glsl.Vec3
reflect33 a b =
    Glsl.unsafeCall2 "reflect" [] a b


reflect44 :
    Glsl.Expression Glsl.Vec4
    -> Glsl.Expression Glsl.Vec4
    -> Glsl.Expression Glsl.Vec4
reflect44 a b =
    Glsl.unsafeCall2 "reflect" [] a b


reflectd1d1 :
    Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.Double
reflectd1d1 a b =
    Glsl.unsafeCall2 "reflect" [] a b


reflectd1wd1 : Glsl.Expression Glsl.Double -> Float -> Glsl.Expression Glsl.Double
reflectd1wd1 a b =
    Glsl.unsafeCall2 "reflect" [] a (Glsl.double1 b)


reflectd2d2 :
    Glsl.Expression Glsl.DVec2
    -> Glsl.Expression Glsl.DVec2
    -> Glsl.Expression Glsl.DVec2
reflectd2d2 a b =
    Glsl.unsafeCall2 "reflect" [] a b


reflectd3d3 :
    Glsl.Expression Glsl.DVec3
    -> Glsl.Expression Glsl.DVec3
    -> Glsl.Expression Glsl.DVec3
reflectd3d3 a b =
    Glsl.unsafeCall2 "reflect" [] a b


reflectd4d4 :
    Glsl.Expression Glsl.DVec4
    -> Glsl.Expression Glsl.DVec4
    -> Glsl.Expression Glsl.DVec4
reflectd4d4 a b =
    Glsl.unsafeCall2 "reflect" [] a b


reflectf1 : Float -> Glsl.Expression Glsl.Float_ -> Glsl.Expression Glsl.Float_
reflectf1 a b =
    Glsl.unsafeCall2 "reflect" [] (Glsl.float1 a) b


reflectff : Float -> Float -> Glsl.Expression Glsl.Float_
reflectff a b =
    Glsl.unsafeCall2 "reflect" [] (Glsl.float1 a) (Glsl.float1 b)


reflectwd1d1 : Float -> Glsl.Expression Glsl.Double -> Glsl.Expression Glsl.Double
reflectwd1d1 a b =
    Glsl.unsafeCall2 "reflect" [] (Glsl.double1 a) b


reflectwd1wd1 : Float -> Float -> Glsl.Expression Glsl.Double
reflectwd1wd1 a b =
    Glsl.unsafeCall2 "reflect" [] (Glsl.double1 a) (Glsl.double1 b)


reflect :
    Glsl.Expression (Glsl.Vec t a)
    -> Glsl.Expression (Glsl.Vec t a)
    -> Glsl.Expression (Glsl.Vec t a)
reflect a b =
    Glsl.unsafeCall2 "reflect" [] a b


refract111 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
refract111 a b c =
    Glsl.unsafeCall3 "refract" [] a b c


refract11f :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
refract11f a b c =
    Glsl.unsafeCall3 "refract" [] a b (Glsl.float1 c)


refract1f1 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
refract1f1 a b c =
    Glsl.unsafeCall3 "refract" [] a (Glsl.float1 b) c


refract1ff : Glsl.Expression Glsl.Float_ -> Float -> Float -> Glsl.Expression Glsl.Float_
refract1ff a b c =
    Glsl.unsafeCall3 "refract" [] a (Glsl.float1 b) (Glsl.float1 c)


refract221 :
    Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec2
refract221 a b c =
    Glsl.unsafeCall3 "refract" [] a b c


refract22f :
    Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
    -> Float
    -> Glsl.Expression Glsl.Vec2
refract22f a b c =
    Glsl.unsafeCall3 "refract" [] a b (Glsl.float1 c)


refract331 :
    Glsl.Expression Glsl.Vec3
    -> Glsl.Expression Glsl.Vec3
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec3
refract331 a b c =
    Glsl.unsafeCall3 "refract" [] a b c


refract33f :
    Glsl.Expression Glsl.Vec3
    -> Glsl.Expression Glsl.Vec3
    -> Float
    -> Glsl.Expression Glsl.Vec3
refract33f a b c =
    Glsl.unsafeCall3 "refract" [] a b (Glsl.float1 c)


refract441 :
    Glsl.Expression Glsl.Vec4
    -> Glsl.Expression Glsl.Vec4
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
refract441 a b c =
    Glsl.unsafeCall3 "refract" [] a b c


refract44f :
    Glsl.Expression Glsl.Vec4
    -> Glsl.Expression Glsl.Vec4
    -> Float
    -> Glsl.Expression Glsl.Vec4
refract44f a b c =
    Glsl.unsafeCall3 "refract" [] a b (Glsl.float1 c)


refractd1d11 :
    Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Double
refractd1d11 a b c =
    Glsl.unsafeCall3 "refract" [] a b c


refractd1d1f :
    Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.Double
    -> Float
    -> Glsl.Expression Glsl.Double
refractd1d1f a b c =
    Glsl.unsafeCall3 "refract" [] a b (Glsl.float1 c)


refractd1wd11 :
    Glsl.Expression Glsl.Double
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Double
refractd1wd11 a b c =
    Glsl.unsafeCall3 "refract" [] a (Glsl.double1 b) c


refractd1wd1f : Glsl.Expression Glsl.Double -> Float -> Float -> Glsl.Expression Glsl.Double
refractd1wd1f a b c =
    Glsl.unsafeCall3 "refract" [] a (Glsl.double1 b) (Glsl.float1 c)


refractd2d21 :
    Glsl.Expression Glsl.DVec2
    -> Glsl.Expression Glsl.DVec2
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.DVec2
refractd2d21 a b c =
    Glsl.unsafeCall3 "refract" [] a b c


refractd2d2f :
    Glsl.Expression Glsl.DVec2
    -> Glsl.Expression Glsl.DVec2
    -> Float
    -> Glsl.Expression Glsl.DVec2
refractd2d2f a b c =
    Glsl.unsafeCall3 "refract" [] a b (Glsl.float1 c)


refractd3d31 :
    Glsl.Expression Glsl.DVec3
    -> Glsl.Expression Glsl.DVec3
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.DVec3
refractd3d31 a b c =
    Glsl.unsafeCall3 "refract" [] a b c


refractd3d3f :
    Glsl.Expression Glsl.DVec3
    -> Glsl.Expression Glsl.DVec3
    -> Float
    -> Glsl.Expression Glsl.DVec3
refractd3d3f a b c =
    Glsl.unsafeCall3 "refract" [] a b (Glsl.float1 c)


refractd4d41 :
    Glsl.Expression Glsl.DVec4
    -> Glsl.Expression Glsl.DVec4
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.DVec4
refractd4d41 a b c =
    Glsl.unsafeCall3 "refract" [] a b c


refractd4d4f :
    Glsl.Expression Glsl.DVec4
    -> Glsl.Expression Glsl.DVec4
    -> Float
    -> Glsl.Expression Glsl.DVec4
refractd4d4f a b c =
    Glsl.unsafeCall3 "refract" [] a b (Glsl.float1 c)


refractf11 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
refractf11 a b c =
    Glsl.unsafeCall3 "refract" [] (Glsl.float1 a) b c


refractf1f : Float -> Glsl.Expression Glsl.Float_ -> Float -> Glsl.Expression Glsl.Float_
refractf1f a b c =
    Glsl.unsafeCall3 "refract" [] (Glsl.float1 a) b (Glsl.float1 c)


refractff1 : Float -> Float -> Glsl.Expression Glsl.Float_ -> Glsl.Expression Glsl.Float_
refractff1 a b c =
    Glsl.unsafeCall3 "refract" [] (Glsl.float1 a) (Glsl.float1 b) c


refractfff : Float -> Float -> Float -> Glsl.Expression Glsl.Float_
refractfff a b c =
    Glsl.unsafeCall3
        "refract"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        (Glsl.float1 c)


refractwd1d11 :
    Float
    -> Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Double
refractwd1d11 a b c =
    Glsl.unsafeCall3 "refract" [] (Glsl.double1 a) b c


refractwd1d1f : Float -> Glsl.Expression Glsl.Double -> Float -> Glsl.Expression Glsl.Double
refractwd1d1f a b c =
    Glsl.unsafeCall3 "refract" [] (Glsl.double1 a) b (Glsl.float1 c)


refractwd1wd11 : Float -> Float -> Glsl.Expression Glsl.Float_ -> Glsl.Expression Glsl.Double
refractwd1wd11 a b c =
    Glsl.unsafeCall3 "refract" [] (Glsl.double1 a) (Glsl.double1 b) c


refractwd1wd1f : Float -> Float -> Float -> Glsl.Expression Glsl.Double
refractwd1wd1f a b c =
    Glsl.unsafeCall3
        "refract"
        []
        (Glsl.double1 a)
        (Glsl.double1 b)
        (Glsl.float1 c)


refract :
    Glsl.Expression (Glsl.Vec t a)
    -> Glsl.Expression (Glsl.Vec t a)
    -> Glsl.Expression (Glsl.Vec Float Glsl.D1)
    -> Glsl.Expression (Glsl.Vec t a)
refract a b c =
    Glsl.unsafeCall3 "refract" [] a b c


round1 : Glsl.Expression Glsl.Float_ -> Glsl.Expression Glsl.Float_
round1 a =
    Glsl.unsafeCall1 "round" [] a


round2 : Glsl.Expression Glsl.Vec2 -> Glsl.Expression Glsl.Vec2
round2 a =
    Glsl.unsafeCall1 "round" [] a


round3 : Glsl.Expression Glsl.Vec3 -> Glsl.Expression Glsl.Vec3
round3 a =
    Glsl.unsafeCall1 "round" [] a


round4 : Glsl.Expression Glsl.Vec4 -> Glsl.Expression Glsl.Vec4
round4 a =
    Glsl.unsafeCall1 "round" [] a


roundd1 : Glsl.Expression Glsl.Double -> Glsl.Expression Glsl.Double
roundd1 a =
    Glsl.unsafeCall1 "round" [] a


roundd2 : Glsl.Expression Glsl.DVec2 -> Glsl.Expression Glsl.DVec2
roundd2 a =
    Glsl.unsafeCall1 "round" [] a


roundd3 : Glsl.Expression Glsl.DVec3 -> Glsl.Expression Glsl.DVec3
roundd3 a =
    Glsl.unsafeCall1 "round" [] a


roundd4 : Glsl.Expression Glsl.DVec4 -> Glsl.Expression Glsl.DVec4
roundd4 a =
    Glsl.unsafeCall1 "round" [] a


roundf : Float -> Glsl.Expression Glsl.Float_
roundf a =
    Glsl.unsafeCall1 "round" [] (Glsl.float1 a)


roundwd1 : Float -> Glsl.Expression Glsl.Double
roundwd1 a =
    Glsl.unsafeCall1 "round" [] (Glsl.double1 a)


roundEven1 : Glsl.Expression Glsl.Float_ -> Glsl.Expression Glsl.Float_
roundEven1 a =
    Glsl.unsafeCall1 "roundEven" [] a


roundEven2 : Glsl.Expression Glsl.Vec2 -> Glsl.Expression Glsl.Vec2
roundEven2 a =
    Glsl.unsafeCall1 "roundEven" [] a


roundEven3 : Glsl.Expression Glsl.Vec3 -> Glsl.Expression Glsl.Vec3
roundEven3 a =
    Glsl.unsafeCall1 "roundEven" [] a


roundEven4 : Glsl.Expression Glsl.Vec4 -> Glsl.Expression Glsl.Vec4
roundEven4 a =
    Glsl.unsafeCall1 "roundEven" [] a


roundEvend1 : Glsl.Expression Glsl.Double -> Glsl.Expression Glsl.Double
roundEvend1 a =
    Glsl.unsafeCall1 "roundEven" [] a


roundEvend2 : Glsl.Expression Glsl.DVec2 -> Glsl.Expression Glsl.DVec2
roundEvend2 a =
    Glsl.unsafeCall1 "roundEven" [] a


roundEvend3 : Glsl.Expression Glsl.DVec3 -> Glsl.Expression Glsl.DVec3
roundEvend3 a =
    Glsl.unsafeCall1 "roundEven" [] a


roundEvend4 : Glsl.Expression Glsl.DVec4 -> Glsl.Expression Glsl.DVec4
roundEvend4 a =
    Glsl.unsafeCall1 "roundEven" [] a


roundEvenf : Float -> Glsl.Expression Glsl.Float_
roundEvenf a =
    Glsl.unsafeCall1 "roundEven" [] (Glsl.float1 a)


roundEvenwd1 : Float -> Glsl.Expression Glsl.Double
roundEvenwd1 a =
    Glsl.unsafeCall1 "roundEven" [] (Glsl.double1 a)


roundEven : Glsl.Expression (Glsl.Vec t a) -> Glsl.Expression (Glsl.Vec t a)
roundEven a =
    Glsl.unsafeCall1 "roundEven" [] a


sign1 : Glsl.Expression Glsl.Float_ -> Glsl.Expression Glsl.Float_
sign1 a =
    Glsl.unsafeCall1 "sign" [] a


sign2 : Glsl.Expression Glsl.Vec2 -> Glsl.Expression Glsl.Vec2
sign2 a =
    Glsl.unsafeCall1 "sign" [] a


sign3 : Glsl.Expression Glsl.Vec3 -> Glsl.Expression Glsl.Vec3
sign3 a =
    Glsl.unsafeCall1 "sign" [] a


sign4 : Glsl.Expression Glsl.Vec4 -> Glsl.Expression Glsl.Vec4
sign4 a =
    Glsl.unsafeCall1 "sign" [] a


signd1 : Glsl.Expression Glsl.Double -> Glsl.Expression Glsl.Double
signd1 a =
    Glsl.unsafeCall1 "sign" [] a


signd2 : Glsl.Expression Glsl.DVec2 -> Glsl.Expression Glsl.DVec2
signd2 a =
    Glsl.unsafeCall1 "sign" [] a


signd3 : Glsl.Expression Glsl.DVec3 -> Glsl.Expression Glsl.DVec3
signd3 a =
    Glsl.unsafeCall1 "sign" [] a


signd4 : Glsl.Expression Glsl.DVec4 -> Glsl.Expression Glsl.DVec4
signd4 a =
    Glsl.unsafeCall1 "sign" [] a


signf : Float -> Glsl.Expression Glsl.Float_
signf a =
    Glsl.unsafeCall1 "sign" [] (Glsl.float1 a)


signi1 : Glsl.Expression Glsl.Int_ -> Glsl.Expression Glsl.Int_
signi1 a =
    Glsl.unsafeCall1 "sign" [] a


signi2 : Glsl.Expression Glsl.IVec2 -> Glsl.Expression Glsl.IVec2
signi2 a =
    Glsl.unsafeCall1 "sign" [] a


signi3 : Glsl.Expression Glsl.IVec3 -> Glsl.Expression Glsl.IVec3
signi3 a =
    Glsl.unsafeCall1 "sign" [] a


signi4 : Glsl.Expression Glsl.IVec4 -> Glsl.Expression Glsl.IVec4
signi4 a =
    Glsl.unsafeCall1 "sign" [] a


signwd1 : Float -> Glsl.Expression Glsl.Double
signwd1 a =
    Glsl.unsafeCall1 "sign" [] (Glsl.double1 a)


signwi1 : Int -> Glsl.Expression Glsl.Int_
signwi1 a =
    Glsl.unsafeCall1 "sign" [] (Glsl.int1 a)


sign : Glsl.Expression (Glsl.Vec t a) -> Glsl.Expression (Glsl.Vec t a)
sign a =
    Glsl.unsafeCall1 "sign" [] a


smoothstep111 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
smoothstep111 a b c =
    Glsl.unsafeCall3 "smoothstep" [] a b c


smoothstep112 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
smoothstep112 a b c =
    Glsl.unsafeCall3 "smoothstep" [] a b c


smoothstep113 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec3
    -> Glsl.Expression Glsl.Vec3
smoothstep113 a b c =
    Glsl.unsafeCall3 "smoothstep" [] a b c


smoothstep114 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
    -> Glsl.Expression Glsl.Vec4
smoothstep114 a b c =
    Glsl.unsafeCall3 "smoothstep" [] a b c


smoothstep11f :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
smoothstep11f a b c =
    Glsl.unsafeCall3 "smoothstep" [] a b (Glsl.float1 c)


smoothstep1f1 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
smoothstep1f1 a b c =
    Glsl.unsafeCall3 "smoothstep" [] a (Glsl.float1 b) c


smoothstep1f2 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
smoothstep1f2 a b c =
    Glsl.unsafeCall3 "smoothstep" [] a (Glsl.float1 b) c


smoothstep1f3 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Vec3
    -> Glsl.Expression Glsl.Vec3
smoothstep1f3 a b c =
    Glsl.unsafeCall3 "smoothstep" [] a (Glsl.float1 b) c


smoothstep1f4 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Vec4
    -> Glsl.Expression Glsl.Vec4
smoothstep1f4 a b c =
    Glsl.unsafeCall3 "smoothstep" [] a (Glsl.float1 b) c


smoothstep1ff : Glsl.Expression Glsl.Float_ -> Float -> Float -> Glsl.Expression Glsl.Float_
smoothstep1ff a b c =
    Glsl.unsafeCall3 "smoothstep" [] a (Glsl.float1 b) (Glsl.float1 c)


smoothstep222 :
    Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
smoothstep222 a b c =
    Glsl.unsafeCall3 "smoothstep" [] a b c


smoothstep333 :
    Glsl.Expression Glsl.Vec3
    -> Glsl.Expression Glsl.Vec3
    -> Glsl.Expression Glsl.Vec3
    -> Glsl.Expression Glsl.Vec3
smoothstep333 a b c =
    Glsl.unsafeCall3 "smoothstep" [] a b c


smoothstep444 :
    Glsl.Expression Glsl.Vec4
    -> Glsl.Expression Glsl.Vec4
    -> Glsl.Expression Glsl.Vec4
    -> Glsl.Expression Glsl.Vec4
smoothstep444 a b c =
    Glsl.unsafeCall3 "smoothstep" [] a b c


smoothstepd1d1d1 :
    Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.Double
smoothstepd1d1d1 a b c =
    Glsl.unsafeCall3 "smoothstep" [] a b c


smoothstepd1d1d2 :
    Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.DVec2
    -> Glsl.Expression Glsl.DVec2
smoothstepd1d1d2 a b c =
    Glsl.unsafeCall3 "smoothstep" [] a b c


smoothstepd1d1d3 :
    Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.DVec3
    -> Glsl.Expression Glsl.DVec3
smoothstepd1d1d3 a b c =
    Glsl.unsafeCall3 "smoothstep" [] a b c


smoothstepd1d1d4 :
    Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.DVec4
    -> Glsl.Expression Glsl.DVec4
smoothstepd1d1d4 a b c =
    Glsl.unsafeCall3 "smoothstep" [] a b c


smoothstepd1d1wd1 :
    Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.Double
    -> Float
    -> Glsl.Expression Glsl.Double
smoothstepd1d1wd1 a b c =
    Glsl.unsafeCall3 "smoothstep" [] a b (Glsl.double1 c)


smoothstepd1wd1d1 :
    Glsl.Expression Glsl.Double
    -> Float
    -> Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.Double
smoothstepd1wd1d1 a b c =
    Glsl.unsafeCall3 "smoothstep" [] a (Glsl.double1 b) c


smoothstepd1wd1d2 :
    Glsl.Expression Glsl.Double
    -> Float
    -> Glsl.Expression Glsl.DVec2
    -> Glsl.Expression Glsl.DVec2
smoothstepd1wd1d2 a b c =
    Glsl.unsafeCall3 "smoothstep" [] a (Glsl.double1 b) c


smoothstepd1wd1d3 :
    Glsl.Expression Glsl.Double
    -> Float
    -> Glsl.Expression Glsl.DVec3
    -> Glsl.Expression Glsl.DVec3
smoothstepd1wd1d3 a b c =
    Glsl.unsafeCall3 "smoothstep" [] a (Glsl.double1 b) c


smoothstepd1wd1d4 :
    Glsl.Expression Glsl.Double
    -> Float
    -> Glsl.Expression Glsl.DVec4
    -> Glsl.Expression Glsl.DVec4
smoothstepd1wd1d4 a b c =
    Glsl.unsafeCall3 "smoothstep" [] a (Glsl.double1 b) c


smoothstepd1wd1wd1 : Glsl.Expression Glsl.Double -> Float -> Float -> Glsl.Expression Glsl.Double
smoothstepd1wd1wd1 a b c =
    Glsl.unsafeCall3 "smoothstep" [] a (Glsl.double1 b) (Glsl.double1 c)


smoothstepd2d2d2 :
    Glsl.Expression Glsl.DVec2
    -> Glsl.Expression Glsl.DVec2
    -> Glsl.Expression Glsl.DVec2
    -> Glsl.Expression Glsl.DVec2
smoothstepd2d2d2 a b c =
    Glsl.unsafeCall3 "smoothstep" [] a b c


smoothstepd3d3d3 :
    Glsl.Expression Glsl.DVec3
    -> Glsl.Expression Glsl.DVec3
    -> Glsl.Expression Glsl.DVec3
    -> Glsl.Expression Glsl.DVec3
smoothstepd3d3d3 a b c =
    Glsl.unsafeCall3 "smoothstep" [] a b c


smoothstepd4d4d4 :
    Glsl.Expression Glsl.DVec4
    -> Glsl.Expression Glsl.DVec4
    -> Glsl.Expression Glsl.DVec4
    -> Glsl.Expression Glsl.DVec4
smoothstepd4d4d4 a b c =
    Glsl.unsafeCall3 "smoothstep" [] a b c


smoothstepf11 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
smoothstepf11 a b c =
    Glsl.unsafeCall3 "smoothstep" [] (Glsl.float1 a) b c


smoothstepf12 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
smoothstepf12 a b c =
    Glsl.unsafeCall3 "smoothstep" [] (Glsl.float1 a) b c


smoothstepf13 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec3
    -> Glsl.Expression Glsl.Vec3
smoothstepf13 a b c =
    Glsl.unsafeCall3 "smoothstep" [] (Glsl.float1 a) b c


smoothstepf14 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
    -> Glsl.Expression Glsl.Vec4
smoothstepf14 a b c =
    Glsl.unsafeCall3 "smoothstep" [] (Glsl.float1 a) b c


smoothstepf1f : Float -> Glsl.Expression Glsl.Float_ -> Float -> Glsl.Expression Glsl.Float_
smoothstepf1f a b c =
    Glsl.unsafeCall3 "smoothstep" [] (Glsl.float1 a) b (Glsl.float1 c)


smoothstepff1 : Float -> Float -> Glsl.Expression Glsl.Float_ -> Glsl.Expression Glsl.Float_
smoothstepff1 a b c =
    Glsl.unsafeCall3 "smoothstep" [] (Glsl.float1 a) (Glsl.float1 b) c


smoothstepff2 : Float -> Float -> Glsl.Expression Glsl.Vec2 -> Glsl.Expression Glsl.Vec2
smoothstepff2 a b c =
    Glsl.unsafeCall3 "smoothstep" [] (Glsl.float1 a) (Glsl.float1 b) c


smoothstepff3 : Float -> Float -> Glsl.Expression Glsl.Vec3 -> Glsl.Expression Glsl.Vec3
smoothstepff3 a b c =
    Glsl.unsafeCall3 "smoothstep" [] (Glsl.float1 a) (Glsl.float1 b) c


smoothstepff4 : Float -> Float -> Glsl.Expression Glsl.Vec4 -> Glsl.Expression Glsl.Vec4
smoothstepff4 a b c =
    Glsl.unsafeCall3 "smoothstep" [] (Glsl.float1 a) (Glsl.float1 b) c


smoothstepfff : Float -> Float -> Float -> Glsl.Expression Glsl.Float_
smoothstepfff a b c =
    Glsl.unsafeCall3
        "smoothstep"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        (Glsl.float1 c)


smoothstepwd1d1d1 :
    Float
    -> Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.Double
smoothstepwd1d1d1 a b c =
    Glsl.unsafeCall3 "smoothstep" [] (Glsl.double1 a) b c


smoothstepwd1d1d2 :
    Float
    -> Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.DVec2
    -> Glsl.Expression Glsl.DVec2
smoothstepwd1d1d2 a b c =
    Glsl.unsafeCall3 "smoothstep" [] (Glsl.double1 a) b c


smoothstepwd1d1d3 :
    Float
    -> Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.DVec3
    -> Glsl.Expression Glsl.DVec3
smoothstepwd1d1d3 a b c =
    Glsl.unsafeCall3 "smoothstep" [] (Glsl.double1 a) b c


smoothstepwd1d1d4 :
    Float
    -> Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.DVec4
    -> Glsl.Expression Glsl.DVec4
smoothstepwd1d1d4 a b c =
    Glsl.unsafeCall3 "smoothstep" [] (Glsl.double1 a) b c


smoothstepwd1d1wd1 : Float -> Glsl.Expression Glsl.Double -> Float -> Glsl.Expression Glsl.Double
smoothstepwd1d1wd1 a b c =
    Glsl.unsafeCall3 "smoothstep" [] (Glsl.double1 a) b (Glsl.double1 c)


smoothstepwd1wd1d1 : Float -> Float -> Glsl.Expression Glsl.Double -> Glsl.Expression Glsl.Double
smoothstepwd1wd1d1 a b c =
    Glsl.unsafeCall3 "smoothstep" [] (Glsl.double1 a) (Glsl.double1 b) c


smoothstepwd1wd1d2 : Float -> Float -> Glsl.Expression Glsl.DVec2 -> Glsl.Expression Glsl.DVec2
smoothstepwd1wd1d2 a b c =
    Glsl.unsafeCall3 "smoothstep" [] (Glsl.double1 a) (Glsl.double1 b) c


smoothstepwd1wd1d3 : Float -> Float -> Glsl.Expression Glsl.DVec3 -> Glsl.Expression Glsl.DVec3
smoothstepwd1wd1d3 a b c =
    Glsl.unsafeCall3 "smoothstep" [] (Glsl.double1 a) (Glsl.double1 b) c


smoothstepwd1wd1d4 : Float -> Float -> Glsl.Expression Glsl.DVec4 -> Glsl.Expression Glsl.DVec4
smoothstepwd1wd1d4 a b c =
    Glsl.unsafeCall3 "smoothstep" [] (Glsl.double1 a) (Glsl.double1 b) c


smoothstepwd1wd1wd1 : Float -> Float -> Float -> Glsl.Expression Glsl.Double
smoothstepwd1wd1wd1 a b c =
    Glsl.unsafeCall3
        "smoothstep"
        []
        (Glsl.double1 a)
        (Glsl.double1 b)
        (Glsl.double1 c)


smoothstep :
    Glsl.Expression (Glsl.Vec t a)
    -> Glsl.Expression (Glsl.Vec t a)
    -> Glsl.Expression (Glsl.Vec t a)
    -> Glsl.Expression (Glsl.Vec t a)
smoothstep a b c =
    Glsl.unsafeCall3 "smoothstep" [] a b c


sqrt1 : Glsl.Expression Glsl.Float_ -> Glsl.Expression Glsl.Float_
sqrt1 a =
    Glsl.unsafeCall1 "sqrt" [] a


sqrt2 : Glsl.Expression Glsl.Vec2 -> Glsl.Expression Glsl.Vec2
sqrt2 a =
    Glsl.unsafeCall1 "sqrt" [] a


sqrt3 : Glsl.Expression Glsl.Vec3 -> Glsl.Expression Glsl.Vec3
sqrt3 a =
    Glsl.unsafeCall1 "sqrt" [] a


sqrt4 : Glsl.Expression Glsl.Vec4 -> Glsl.Expression Glsl.Vec4
sqrt4 a =
    Glsl.unsafeCall1 "sqrt" [] a


sqrtd1 : Glsl.Expression Glsl.Double -> Glsl.Expression Glsl.Double
sqrtd1 a =
    Glsl.unsafeCall1 "sqrt" [] a


sqrtd2 : Glsl.Expression Glsl.DVec2 -> Glsl.Expression Glsl.DVec2
sqrtd2 a =
    Glsl.unsafeCall1 "sqrt" [] a


sqrtd3 : Glsl.Expression Glsl.DVec3 -> Glsl.Expression Glsl.DVec3
sqrtd3 a =
    Glsl.unsafeCall1 "sqrt" [] a


sqrtd4 : Glsl.Expression Glsl.DVec4 -> Glsl.Expression Glsl.DVec4
sqrtd4 a =
    Glsl.unsafeCall1 "sqrt" [] a


sqrtf : Float -> Glsl.Expression Glsl.Float_
sqrtf a =
    Glsl.unsafeCall1 "sqrt" [] (Glsl.float1 a)


sqrtwd1 : Float -> Glsl.Expression Glsl.Double
sqrtwd1 a =
    Glsl.unsafeCall1 "sqrt" [] (Glsl.double1 a)


step11 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
step11 a b =
    Glsl.unsafeCall2 "step" [] a b


step12 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
step12 a b =
    Glsl.unsafeCall2 "step" [] a b


step13 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec3
    -> Glsl.Expression Glsl.Vec3
step13 a b =
    Glsl.unsafeCall2 "step" [] a b


step14 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
    -> Glsl.Expression Glsl.Vec4
step14 a b =
    Glsl.unsafeCall2 "step" [] a b


step1f : Glsl.Expression Glsl.Float_ -> Float -> Glsl.Expression Glsl.Float_
step1f a b =
    Glsl.unsafeCall2 "step" [] a (Glsl.float1 b)


step22 :
    Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
step22 a b =
    Glsl.unsafeCall2 "step" [] a b


step33 :
    Glsl.Expression Glsl.Vec3
    -> Glsl.Expression Glsl.Vec3
    -> Glsl.Expression Glsl.Vec3
step33 a b =
    Glsl.unsafeCall2 "step" [] a b


step44 :
    Glsl.Expression Glsl.Vec4
    -> Glsl.Expression Glsl.Vec4
    -> Glsl.Expression Glsl.Vec4
step44 a b =
    Glsl.unsafeCall2 "step" [] a b


stepd1d1 :
    Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.Double
stepd1d1 a b =
    Glsl.unsafeCall2 "step" [] a b


stepd1d2 :
    Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.DVec2
    -> Glsl.Expression Glsl.DVec2
stepd1d2 a b =
    Glsl.unsafeCall2 "step" [] a b


stepd1d3 :
    Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.DVec3
    -> Glsl.Expression Glsl.DVec3
stepd1d3 a b =
    Glsl.unsafeCall2 "step" [] a b


stepd1d4 :
    Glsl.Expression Glsl.Double
    -> Glsl.Expression Glsl.DVec4
    -> Glsl.Expression Glsl.DVec4
stepd1d4 a b =
    Glsl.unsafeCall2 "step" [] a b


stepd1wd1 : Glsl.Expression Glsl.Double -> Float -> Glsl.Expression Glsl.Double
stepd1wd1 a b =
    Glsl.unsafeCall2 "step" [] a (Glsl.double1 b)


stepd2d2 :
    Glsl.Expression Glsl.DVec2
    -> Glsl.Expression Glsl.DVec2
    -> Glsl.Expression Glsl.DVec2
stepd2d2 a b =
    Glsl.unsafeCall2 "step" [] a b


stepd3d3 :
    Glsl.Expression Glsl.DVec3
    -> Glsl.Expression Glsl.DVec3
    -> Glsl.Expression Glsl.DVec3
stepd3d3 a b =
    Glsl.unsafeCall2 "step" [] a b


stepd4d4 :
    Glsl.Expression Glsl.DVec4
    -> Glsl.Expression Glsl.DVec4
    -> Glsl.Expression Glsl.DVec4
stepd4d4 a b =
    Glsl.unsafeCall2 "step" [] a b


stepf1 : Float -> Glsl.Expression Glsl.Float_ -> Glsl.Expression Glsl.Float_
stepf1 a b =
    Glsl.unsafeCall2 "step" [] (Glsl.float1 a) b


stepf2 : Float -> Glsl.Expression Glsl.Vec2 -> Glsl.Expression Glsl.Vec2
stepf2 a b =
    Glsl.unsafeCall2 "step" [] (Glsl.float1 a) b


stepf3 : Float -> Glsl.Expression Glsl.Vec3 -> Glsl.Expression Glsl.Vec3
stepf3 a b =
    Glsl.unsafeCall2 "step" [] (Glsl.float1 a) b


stepf4 : Float -> Glsl.Expression Glsl.Vec4 -> Glsl.Expression Glsl.Vec4
stepf4 a b =
    Glsl.unsafeCall2 "step" [] (Glsl.float1 a) b


stepff : Float -> Float -> Glsl.Expression Glsl.Float_
stepff a b =
    Glsl.unsafeCall2 "step" [] (Glsl.float1 a) (Glsl.float1 b)


stepwd1d1 : Float -> Glsl.Expression Glsl.Double -> Glsl.Expression Glsl.Double
stepwd1d1 a b =
    Glsl.unsafeCall2 "step" [] (Glsl.double1 a) b


stepwd1d2 : Float -> Glsl.Expression Glsl.DVec2 -> Glsl.Expression Glsl.DVec2
stepwd1d2 a b =
    Glsl.unsafeCall2 "step" [] (Glsl.double1 a) b


stepwd1d3 : Float -> Glsl.Expression Glsl.DVec3 -> Glsl.Expression Glsl.DVec3
stepwd1d3 a b =
    Glsl.unsafeCall2 "step" [] (Glsl.double1 a) b


stepwd1d4 : Float -> Glsl.Expression Glsl.DVec4 -> Glsl.Expression Glsl.DVec4
stepwd1d4 a b =
    Glsl.unsafeCall2 "step" [] (Glsl.double1 a) b


stepwd1wd1 : Float -> Float -> Glsl.Expression Glsl.Double
stepwd1wd1 a b =
    Glsl.unsafeCall2 "step" [] (Glsl.double1 a) (Glsl.double1 b)


step :
    Glsl.Expression (Glsl.Vec t a)
    -> Glsl.Expression (Glsl.Vec t a)
    -> Glsl.Expression (Glsl.Vec t a)
step a b =
    Glsl.unsafeCall2 "step" [] a b


trunc1 : Glsl.Expression Glsl.Float_ -> Glsl.Expression Glsl.Float_
trunc1 a =
    Glsl.unsafeCall1 "trunc" [] a


trunc2 : Glsl.Expression Glsl.Vec2 -> Glsl.Expression Glsl.Vec2
trunc2 a =
    Glsl.unsafeCall1 "trunc" [] a


trunc3 : Glsl.Expression Glsl.Vec3 -> Glsl.Expression Glsl.Vec3
trunc3 a =
    Glsl.unsafeCall1 "trunc" [] a


trunc4 : Glsl.Expression Glsl.Vec4 -> Glsl.Expression Glsl.Vec4
trunc4 a =
    Glsl.unsafeCall1 "trunc" [] a


truncd1 : Glsl.Expression Glsl.Double -> Glsl.Expression Glsl.Double
truncd1 a =
    Glsl.unsafeCall1 "trunc" [] a


truncd2 : Glsl.Expression Glsl.DVec2 -> Glsl.Expression Glsl.DVec2
truncd2 a =
    Glsl.unsafeCall1 "trunc" [] a


truncd3 : Glsl.Expression Glsl.DVec3 -> Glsl.Expression Glsl.DVec3
truncd3 a =
    Glsl.unsafeCall1 "trunc" [] a


truncd4 : Glsl.Expression Glsl.DVec4 -> Glsl.Expression Glsl.DVec4
truncd4 a =
    Glsl.unsafeCall1 "trunc" [] a


truncf : Float -> Glsl.Expression Glsl.Float_
truncf a =
    Glsl.unsafeCall1 "trunc" [] (Glsl.float1 a)


truncwd1 : Float -> Glsl.Expression Glsl.Double
truncwd1 a =
    Glsl.unsafeCall1 "trunc" [] (Glsl.double1 a)


trunc : Glsl.Expression (Glsl.Vec t a) -> Glsl.Expression (Glsl.Vec t a)
trunc a =
    Glsl.unsafeCall1 "trunc" [] a


uintBitsToFloatu1 : Glsl.Expression Glsl.Uint -> Glsl.Expression Glsl.Float_
uintBitsToFloatu1 a =
    Glsl.unsafeCall1 "uintBitsToFloat" [] a


uintBitsToFloatu2 : Glsl.Expression Glsl.UVec2 -> Glsl.Expression Glsl.Vec2
uintBitsToFloatu2 a =
    Glsl.unsafeCall1 "uintBitsToFloat" [] a


uintBitsToFloatu3 : Glsl.Expression Glsl.UVec3 -> Glsl.Expression Glsl.Vec3
uintBitsToFloatu3 a =
    Glsl.unsafeCall1 "uintBitsToFloat" [] a


uintBitsToFloatu4 : Glsl.Expression Glsl.UVec4 -> Glsl.Expression Glsl.Vec4
uintBitsToFloatu4 a =
    Glsl.unsafeCall1 "uintBitsToFloat" [] a


uintBitsToFloatwu1 : Int -> Glsl.Expression Glsl.Float_
uintBitsToFloatwu1 a =
    Glsl.unsafeCall1 "uintBitsToFloat" [] (Glsl.uint1 a)


vec21 : Glsl.Expression Glsl.Float_ -> Glsl.Expression Glsl.Vec2
vec21 a =
    Glsl.unsafeCall1 "vec2" [] a


vec211 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec2
vec211 a b =
    Glsl.unsafeCall2 "vec2" [] a b


vec21f : Glsl.Expression Glsl.Float_ -> Float -> Glsl.Expression Glsl.Vec2
vec21f a b =
    Glsl.unsafeCall2 "vec2" [] a (Glsl.float1 b)


vec21i1 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec2
vec21i1 a b =
    Glsl.unsafeCall2 "vec2" [] a b


vec21wi1 : Glsl.Expression Glsl.Float_ -> Int -> Glsl.Expression Glsl.Vec2
vec21wi1 a b =
    Glsl.unsafeCall2 "vec2" [] a (Glsl.int1 b)


vec2f : Float -> Glsl.Expression Glsl.Vec2
vec2f a =
    Glsl.unsafeCall1 "vec2" [] (Glsl.float1 a)


vec2f1 : Float -> Glsl.Expression Glsl.Float_ -> Glsl.Expression Glsl.Vec2
vec2f1 a b =
    Glsl.unsafeCall2 "vec2" [] (Glsl.float1 a) b


vec2ff : Float -> Float -> Glsl.Expression Glsl.Vec2
vec2ff a b =
    Glsl.unsafeCall2 "vec2" [] (Glsl.float1 a) (Glsl.float1 b)


vec2fi1 : Float -> Glsl.Expression Glsl.Int_ -> Glsl.Expression Glsl.Vec2
vec2fi1 a b =
    Glsl.unsafeCall2 "vec2" [] (Glsl.float1 a) b


vec2fwi1 : Float -> Int -> Glsl.Expression Glsl.Vec2
vec2fwi1 a b =
    Glsl.unsafeCall2 "vec2" [] (Glsl.float1 a) (Glsl.int1 b)


vec2i1 : Glsl.Expression Glsl.Int_ -> Glsl.Expression Glsl.Vec2
vec2i1 a =
    Glsl.unsafeCall1 "vec2" [] a


vec2i11 :
    Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec2
vec2i11 a b =
    Glsl.unsafeCall2 "vec2" [] a b


vec2i1f : Glsl.Expression Glsl.Int_ -> Float -> Glsl.Expression Glsl.Vec2
vec2i1f a b =
    Glsl.unsafeCall2 "vec2" [] a (Glsl.float1 b)


vec2i1i1 :
    Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec2
vec2i1i1 a b =
    Glsl.unsafeCall2 "vec2" [] a b


vec2i1wi1 : Glsl.Expression Glsl.Int_ -> Int -> Glsl.Expression Glsl.Vec2
vec2i1wi1 a b =
    Glsl.unsafeCall2 "vec2" [] a (Glsl.int1 b)


vec2wi1 : Int -> Glsl.Expression Glsl.Vec2
vec2wi1 a =
    Glsl.unsafeCall1 "vec2" [] (Glsl.int1 a)


vec2wi11 : Int -> Glsl.Expression Glsl.Float_ -> Glsl.Expression Glsl.Vec2
vec2wi11 a b =
    Glsl.unsafeCall2 "vec2" [] (Glsl.int1 a) b


vec2wi1f : Int -> Float -> Glsl.Expression Glsl.Vec2
vec2wi1f a b =
    Glsl.unsafeCall2 "vec2" [] (Glsl.int1 a) (Glsl.float1 b)


vec2wi1i1 : Int -> Glsl.Expression Glsl.Int_ -> Glsl.Expression Glsl.Vec2
vec2wi1i1 a b =
    Glsl.unsafeCall2 "vec2" [] (Glsl.int1 a) b


vec2wi1wi1 : Int -> Int -> Glsl.Expression Glsl.Vec2
vec2wi1wi1 a b =
    Glsl.unsafeCall2 "vec2" [] (Glsl.int1 a) (Glsl.int1 b)


vec31 : Glsl.Expression Glsl.Float_ -> Glsl.Expression Glsl.Vec3
vec31 a =
    Glsl.unsafeCall1 "vec3" [] a


vec3111 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec3
vec3111 a b c =
    Glsl.unsafeCall3 "vec3" [] a b c


vec311f :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Vec3
vec311f a b c =
    Glsl.unsafeCall3 "vec3" [] a b (Glsl.float1 c)


vec311i1 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec3
vec311i1 a b c =
    Glsl.unsafeCall3 "vec3" [] a b c


vec311wi1 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Int
    -> Glsl.Expression Glsl.Vec3
vec311wi1 a b c =
    Glsl.unsafeCall3 "vec3" [] a b (Glsl.int1 c)


vec312 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec3
vec312 a b =
    Glsl.unsafeCall2 "vec3" [] a b


vec31f1 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec3
vec31f1 a b c =
    Glsl.unsafeCall3 "vec3" [] a (Glsl.float1 b) c


vec31ff : Glsl.Expression Glsl.Float_ -> Float -> Float -> Glsl.Expression Glsl.Vec3
vec31ff a b c =
    Glsl.unsafeCall3 "vec3" [] a (Glsl.float1 b) (Glsl.float1 c)


vec31fi1 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec3
vec31fi1 a b c =
    Glsl.unsafeCall3 "vec3" [] a (Glsl.float1 b) c


vec31fwi1 : Glsl.Expression Glsl.Float_ -> Float -> Int -> Glsl.Expression Glsl.Vec3
vec31fwi1 a b c =
    Glsl.unsafeCall3 "vec3" [] a (Glsl.float1 b) (Glsl.int1 c)


vec31i11 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec3
vec31i11 a b c =
    Glsl.unsafeCall3 "vec3" [] a b c


vec31i1f :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Int_
    -> Float
    -> Glsl.Expression Glsl.Vec3
vec31i1f a b c =
    Glsl.unsafeCall3 "vec3" [] a b (Glsl.float1 c)


vec31i1i1 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec3
vec31i1i1 a b c =
    Glsl.unsafeCall3 "vec3" [] a b c


vec31i1wi1 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Int_
    -> Int
    -> Glsl.Expression Glsl.Vec3
vec31i1wi1 a b c =
    Glsl.unsafeCall3 "vec3" [] a b (Glsl.int1 c)


vec31wi11 :
    Glsl.Expression Glsl.Float_
    -> Int
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec3
vec31wi11 a b c =
    Glsl.unsafeCall3 "vec3" [] a (Glsl.int1 b) c


vec31wi1f : Glsl.Expression Glsl.Float_ -> Int -> Float -> Glsl.Expression Glsl.Vec3
vec31wi1f a b c =
    Glsl.unsafeCall3 "vec3" [] a (Glsl.int1 b) (Glsl.float1 c)


vec31wi1i1 :
    Glsl.Expression Glsl.Float_
    -> Int
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec3
vec31wi1i1 a b c =
    Glsl.unsafeCall3 "vec3" [] a (Glsl.int1 b) c


vec31wi1wi1 : Glsl.Expression Glsl.Float_ -> Int -> Int -> Glsl.Expression Glsl.Vec3
vec31wi1wi1 a b c =
    Glsl.unsafeCall3 "vec3" [] a (Glsl.int1 b) (Glsl.int1 c)


vec321 :
    Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec3
vec321 a b =
    Glsl.unsafeCall2 "vec3" [] a b


vec32f : Glsl.Expression Glsl.Vec2 -> Float -> Glsl.Expression Glsl.Vec3
vec32f a b =
    Glsl.unsafeCall2 "vec3" [] a (Glsl.float1 b)


vec3f : Float -> Glsl.Expression Glsl.Vec3
vec3f a =
    Glsl.unsafeCall1 "vec3" [] (Glsl.float1 a)


vec3f11 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec3
vec3f11 a b c =
    Glsl.unsafeCall3 "vec3" [] (Glsl.float1 a) b c


vec3f1f : Float -> Glsl.Expression Glsl.Float_ -> Float -> Glsl.Expression Glsl.Vec3
vec3f1f a b c =
    Glsl.unsafeCall3 "vec3" [] (Glsl.float1 a) b (Glsl.float1 c)


vec3f1i1 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec3
vec3f1i1 a b c =
    Glsl.unsafeCall3 "vec3" [] (Glsl.float1 a) b c


vec3f1wi1 : Float -> Glsl.Expression Glsl.Float_ -> Int -> Glsl.Expression Glsl.Vec3
vec3f1wi1 a b c =
    Glsl.unsafeCall3 "vec3" [] (Glsl.float1 a) b (Glsl.int1 c)


vec3f2 : Float -> Glsl.Expression Glsl.Vec2 -> Glsl.Expression Glsl.Vec3
vec3f2 a b =
    Glsl.unsafeCall2 "vec3" [] (Glsl.float1 a) b


vec3ff1 : Float -> Float -> Glsl.Expression Glsl.Float_ -> Glsl.Expression Glsl.Vec3
vec3ff1 a b c =
    Glsl.unsafeCall3 "vec3" [] (Glsl.float1 a) (Glsl.float1 b) c


vec3fff : Float -> Float -> Float -> Glsl.Expression Glsl.Vec3
vec3fff a b c =
    Glsl.unsafeCall3 "vec3" [] (Glsl.float1 a) (Glsl.float1 b) (Glsl.float1 c)


vec3ffi1 : Float -> Float -> Glsl.Expression Glsl.Int_ -> Glsl.Expression Glsl.Vec3
vec3ffi1 a b c =
    Glsl.unsafeCall3 "vec3" [] (Glsl.float1 a) (Glsl.float1 b) c


vec3ffwi1 : Float -> Float -> Int -> Glsl.Expression Glsl.Vec3
vec3ffwi1 a b c =
    Glsl.unsafeCall3 "vec3" [] (Glsl.float1 a) (Glsl.float1 b) (Glsl.int1 c)


vec3fi11 :
    Float
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec3
vec3fi11 a b c =
    Glsl.unsafeCall3 "vec3" [] (Glsl.float1 a) b c


vec3fi1f : Float -> Glsl.Expression Glsl.Int_ -> Float -> Glsl.Expression Glsl.Vec3
vec3fi1f a b c =
    Glsl.unsafeCall3 "vec3" [] (Glsl.float1 a) b (Glsl.float1 c)


vec3fi1i1 :
    Float
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec3
vec3fi1i1 a b c =
    Glsl.unsafeCall3 "vec3" [] (Glsl.float1 a) b c


vec3fi1wi1 : Float -> Glsl.Expression Glsl.Int_ -> Int -> Glsl.Expression Glsl.Vec3
vec3fi1wi1 a b c =
    Glsl.unsafeCall3 "vec3" [] (Glsl.float1 a) b (Glsl.int1 c)


vec3fwi11 : Float -> Int -> Glsl.Expression Glsl.Float_ -> Glsl.Expression Glsl.Vec3
vec3fwi11 a b c =
    Glsl.unsafeCall3 "vec3" [] (Glsl.float1 a) (Glsl.int1 b) c


vec3fwi1f : Float -> Int -> Float -> Glsl.Expression Glsl.Vec3
vec3fwi1f a b c =
    Glsl.unsafeCall3 "vec3" [] (Glsl.float1 a) (Glsl.int1 b) (Glsl.float1 c)


vec3fwi1i1 : Float -> Int -> Glsl.Expression Glsl.Int_ -> Glsl.Expression Glsl.Vec3
vec3fwi1i1 a b c =
    Glsl.unsafeCall3 "vec3" [] (Glsl.float1 a) (Glsl.int1 b) c


vec3fwi1wi1 : Float -> Int -> Int -> Glsl.Expression Glsl.Vec3
vec3fwi1wi1 a b c =
    Glsl.unsafeCall3 "vec3" [] (Glsl.float1 a) (Glsl.int1 b) (Glsl.int1 c)


vec3i1 : Glsl.Expression Glsl.Int_ -> Glsl.Expression Glsl.Vec3
vec3i1 a =
    Glsl.unsafeCall1 "vec3" [] a


vec3i111 :
    Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec3
vec3i111 a b c =
    Glsl.unsafeCall3 "vec3" [] a b c


vec3i11f :
    Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Vec3
vec3i11f a b c =
    Glsl.unsafeCall3 "vec3" [] a b (Glsl.float1 c)


vec3i11i1 :
    Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec3
vec3i11i1 a b c =
    Glsl.unsafeCall3 "vec3" [] a b c


vec3i11wi1 :
    Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Float_
    -> Int
    -> Glsl.Expression Glsl.Vec3
vec3i11wi1 a b c =
    Glsl.unsafeCall3 "vec3" [] a b (Glsl.int1 c)


vec3i1f1 :
    Glsl.Expression Glsl.Int_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec3
vec3i1f1 a b c =
    Glsl.unsafeCall3 "vec3" [] a (Glsl.float1 b) c


vec3i1ff : Glsl.Expression Glsl.Int_ -> Float -> Float -> Glsl.Expression Glsl.Vec3
vec3i1ff a b c =
    Glsl.unsafeCall3 "vec3" [] a (Glsl.float1 b) (Glsl.float1 c)


vec3i1fi1 :
    Glsl.Expression Glsl.Int_
    -> Float
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec3
vec3i1fi1 a b c =
    Glsl.unsafeCall3 "vec3" [] a (Glsl.float1 b) c


vec3i1fwi1 : Glsl.Expression Glsl.Int_ -> Float -> Int -> Glsl.Expression Glsl.Vec3
vec3i1fwi1 a b c =
    Glsl.unsafeCall3 "vec3" [] a (Glsl.float1 b) (Glsl.int1 c)


vec3i1i11 :
    Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec3
vec3i1i11 a b c =
    Glsl.unsafeCall3 "vec3" [] a b c


vec3i1i1f :
    Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Float
    -> Glsl.Expression Glsl.Vec3
vec3i1i1f a b c =
    Glsl.unsafeCall3 "vec3" [] a b (Glsl.float1 c)


vec3i1i1i1 :
    Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec3
vec3i1i1i1 a b c =
    Glsl.unsafeCall3 "vec3" [] a b c


vec3i1i1wi1 :
    Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Int
    -> Glsl.Expression Glsl.Vec3
vec3i1i1wi1 a b c =
    Glsl.unsafeCall3 "vec3" [] a b (Glsl.int1 c)


vec3i1wi11 :
    Glsl.Expression Glsl.Int_
    -> Int
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec3
vec3i1wi11 a b c =
    Glsl.unsafeCall3 "vec3" [] a (Glsl.int1 b) c


vec3i1wi1f : Glsl.Expression Glsl.Int_ -> Int -> Float -> Glsl.Expression Glsl.Vec3
vec3i1wi1f a b c =
    Glsl.unsafeCall3 "vec3" [] a (Glsl.int1 b) (Glsl.float1 c)


vec3i1wi1i1 :
    Glsl.Expression Glsl.Int_
    -> Int
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec3
vec3i1wi1i1 a b c =
    Glsl.unsafeCall3 "vec3" [] a (Glsl.int1 b) c


vec3i1wi1wi1 : Glsl.Expression Glsl.Int_ -> Int -> Int -> Glsl.Expression Glsl.Vec3
vec3i1wi1wi1 a b c =
    Glsl.unsafeCall3 "vec3" [] a (Glsl.int1 b) (Glsl.int1 c)


vec3wi1 : Int -> Glsl.Expression Glsl.Vec3
vec3wi1 a =
    Glsl.unsafeCall1 "vec3" [] (Glsl.int1 a)


vec3wi111 :
    Int
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec3
vec3wi111 a b c =
    Glsl.unsafeCall3 "vec3" [] (Glsl.int1 a) b c


vec3wi11f : Int -> Glsl.Expression Glsl.Float_ -> Float -> Glsl.Expression Glsl.Vec3
vec3wi11f a b c =
    Glsl.unsafeCall3 "vec3" [] (Glsl.int1 a) b (Glsl.float1 c)


vec3wi11i1 :
    Int
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec3
vec3wi11i1 a b c =
    Glsl.unsafeCall3 "vec3" [] (Glsl.int1 a) b c


vec3wi11wi1 : Int -> Glsl.Expression Glsl.Float_ -> Int -> Glsl.Expression Glsl.Vec3
vec3wi11wi1 a b c =
    Glsl.unsafeCall3 "vec3" [] (Glsl.int1 a) b (Glsl.int1 c)


vec3wi1f1 : Int -> Float -> Glsl.Expression Glsl.Float_ -> Glsl.Expression Glsl.Vec3
vec3wi1f1 a b c =
    Glsl.unsafeCall3 "vec3" [] (Glsl.int1 a) (Glsl.float1 b) c


vec3wi1ff : Int -> Float -> Float -> Glsl.Expression Glsl.Vec3
vec3wi1ff a b c =
    Glsl.unsafeCall3 "vec3" [] (Glsl.int1 a) (Glsl.float1 b) (Glsl.float1 c)


vec3wi1fi1 : Int -> Float -> Glsl.Expression Glsl.Int_ -> Glsl.Expression Glsl.Vec3
vec3wi1fi1 a b c =
    Glsl.unsafeCall3 "vec3" [] (Glsl.int1 a) (Glsl.float1 b) c


vec3wi1fwi1 : Int -> Float -> Int -> Glsl.Expression Glsl.Vec3
vec3wi1fwi1 a b c =
    Glsl.unsafeCall3 "vec3" [] (Glsl.int1 a) (Glsl.float1 b) (Glsl.int1 c)


vec3wi1i11 :
    Int
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec3
vec3wi1i11 a b c =
    Glsl.unsafeCall3 "vec3" [] (Glsl.int1 a) b c


vec3wi1i1f : Int -> Glsl.Expression Glsl.Int_ -> Float -> Glsl.Expression Glsl.Vec3
vec3wi1i1f a b c =
    Glsl.unsafeCall3 "vec3" [] (Glsl.int1 a) b (Glsl.float1 c)


vec3wi1i1i1 :
    Int
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec3
vec3wi1i1i1 a b c =
    Glsl.unsafeCall3 "vec3" [] (Glsl.int1 a) b c


vec3wi1i1wi1 : Int -> Glsl.Expression Glsl.Int_ -> Int -> Glsl.Expression Glsl.Vec3
vec3wi1i1wi1 a b c =
    Glsl.unsafeCall3 "vec3" [] (Glsl.int1 a) b (Glsl.int1 c)


vec3wi1wi11 : Int -> Int -> Glsl.Expression Glsl.Float_ -> Glsl.Expression Glsl.Vec3
vec3wi1wi11 a b c =
    Glsl.unsafeCall3 "vec3" [] (Glsl.int1 a) (Glsl.int1 b) c


vec3wi1wi1f : Int -> Int -> Float -> Glsl.Expression Glsl.Vec3
vec3wi1wi1f a b c =
    Glsl.unsafeCall3 "vec3" [] (Glsl.int1 a) (Glsl.int1 b) (Glsl.float1 c)


vec3wi1wi1i1 : Int -> Int -> Glsl.Expression Glsl.Int_ -> Glsl.Expression Glsl.Vec3
vec3wi1wi1i1 a b c =
    Glsl.unsafeCall3 "vec3" [] (Glsl.int1 a) (Glsl.int1 b) c


vec3wi1wi1wi1 : Int -> Int -> Int -> Glsl.Expression Glsl.Vec3
vec3wi1wi1wi1 a b c =
    Glsl.unsafeCall3 "vec3" [] (Glsl.int1 a) (Glsl.int1 b) (Glsl.int1 c)


vec41 : Glsl.Expression Glsl.Float_ -> Glsl.Expression Glsl.Vec4
vec41 a =
    Glsl.unsafeCall1 "vec4" [] a


vec41111 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
vec41111 a b c d =
    Glsl.unsafeCall4 "vec4" [] a b c d


vec4111f :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Vec4
vec4111f a b c d =
    Glsl.unsafeCall4 "vec4" [] a b c (Glsl.float1 d)


vec4111i1 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec4
vec4111i1 a b c d =
    Glsl.unsafeCall4 "vec4" [] a b c d


vec4111wi1 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Int
    -> Glsl.Expression Glsl.Vec4
vec4111wi1 a b c d =
    Glsl.unsafeCall4 "vec4" [] a b c (Glsl.int1 d)


vec411f1 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
vec411f1 a b c d =
    Glsl.unsafeCall4 "vec4" [] a b (Glsl.float1 c) d


vec411ff :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Vec4
vec411ff a b c d =
    Glsl.unsafeCall4 "vec4" [] a b (Glsl.float1 c) (Glsl.float1 d)


vec411fi1 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec4
vec411fi1 a b c d =
    Glsl.unsafeCall4 "vec4" [] a b (Glsl.float1 c) d


vec411fwi1 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Int
    -> Glsl.Expression Glsl.Vec4
vec411fwi1 a b c d =
    Glsl.unsafeCall4 "vec4" [] a b (Glsl.float1 c) (Glsl.int1 d)


vec411i11 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
vec411i11 a b c d =
    Glsl.unsafeCall4 "vec4" [] a b c d


vec411i1f :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Int_
    -> Float
    -> Glsl.Expression Glsl.Vec4
vec411i1f a b c d =
    Glsl.unsafeCall4 "vec4" [] a b c (Glsl.float1 d)


vec411i1i1 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec4
vec411i1i1 a b c d =
    Glsl.unsafeCall4 "vec4" [] a b c d


vec411i1wi1 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Int_
    -> Int
    -> Glsl.Expression Glsl.Vec4
vec411i1wi1 a b c d =
    Glsl.unsafeCall4 "vec4" [] a b c (Glsl.int1 d)


vec411wi11 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Int
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
vec411wi11 a b c d =
    Glsl.unsafeCall4 "vec4" [] a b (Glsl.int1 c) d


vec411wi1f :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Int
    -> Float
    -> Glsl.Expression Glsl.Vec4
vec411wi1f a b c d =
    Glsl.unsafeCall4 "vec4" [] a b (Glsl.int1 c) (Glsl.float1 d)


vec411wi1i1 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Int
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec4
vec411wi1i1 a b c d =
    Glsl.unsafeCall4 "vec4" [] a b (Glsl.int1 c) d


vec411wi1wi1 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Int
    -> Int
    -> Glsl.Expression Glsl.Vec4
vec411wi1wi1 a b c d =
    Glsl.unsafeCall4 "vec4" [] a b (Glsl.int1 c) (Glsl.int1 d)


vec413 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec3
    -> Glsl.Expression Glsl.Vec4
vec413 a b =
    Glsl.unsafeCall2 "vec4" [] a b


vec41f11 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
vec41f11 a b c d =
    Glsl.unsafeCall4 "vec4" [] a (Glsl.float1 b) c d


vec41f1f :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Vec4
vec41f1f a b c d =
    Glsl.unsafeCall4 "vec4" [] a (Glsl.float1 b) c (Glsl.float1 d)


vec41f1i1 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec4
vec41f1i1 a b c d =
    Glsl.unsafeCall4 "vec4" [] a (Glsl.float1 b) c d


vec41f1wi1 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Int
    -> Glsl.Expression Glsl.Vec4
vec41f1wi1 a b c d =
    Glsl.unsafeCall4 "vec4" [] a (Glsl.float1 b) c (Glsl.int1 d)


vec41ff1 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
vec41ff1 a b c d =
    Glsl.unsafeCall4 "vec4" [] a (Glsl.float1 b) (Glsl.float1 c) d


vec41fff :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Vec4
vec41fff a b c d =
    Glsl.unsafeCall4 "vec4" [] a (Glsl.float1 b) (Glsl.float1 c) (Glsl.float1 d)


vec41ffi1 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec4
vec41ffi1 a b c d =
    Glsl.unsafeCall4 "vec4" [] a (Glsl.float1 b) (Glsl.float1 c) d


vec41ffwi1 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Int
    -> Glsl.Expression Glsl.Vec4
vec41ffwi1 a b c d =
    Glsl.unsafeCall4 "vec4" [] a (Glsl.float1 b) (Glsl.float1 c) (Glsl.int1 d)


vec41fi11 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
vec41fi11 a b c d =
    Glsl.unsafeCall4 "vec4" [] a (Glsl.float1 b) c d


vec41fi1f :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Int_
    -> Float
    -> Glsl.Expression Glsl.Vec4
vec41fi1f a b c d =
    Glsl.unsafeCall4 "vec4" [] a (Glsl.float1 b) c (Glsl.float1 d)


vec41fi1i1 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec4
vec41fi1i1 a b c d =
    Glsl.unsafeCall4 "vec4" [] a (Glsl.float1 b) c d


vec41fi1wi1 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Int_
    -> Int
    -> Glsl.Expression Glsl.Vec4
vec41fi1wi1 a b c d =
    Glsl.unsafeCall4 "vec4" [] a (Glsl.float1 b) c (Glsl.int1 d)


vec41fwi11 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Int
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
vec41fwi11 a b c d =
    Glsl.unsafeCall4 "vec4" [] a (Glsl.float1 b) (Glsl.int1 c) d


vec41fwi1f :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Int
    -> Float
    -> Glsl.Expression Glsl.Vec4
vec41fwi1f a b c d =
    Glsl.unsafeCall4 "vec4" [] a (Glsl.float1 b) (Glsl.int1 c) (Glsl.float1 d)


vec41fwi1i1 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Int
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec4
vec41fwi1i1 a b c d =
    Glsl.unsafeCall4 "vec4" [] a (Glsl.float1 b) (Glsl.int1 c) d


vec41fwi1wi1 :
    Glsl.Expression Glsl.Float_
    -> Float
    -> Int
    -> Int
    -> Glsl.Expression Glsl.Vec4
vec41fwi1wi1 a b c d =
    Glsl.unsafeCall4 "vec4" [] a (Glsl.float1 b) (Glsl.int1 c) (Glsl.int1 d)


vec41i111 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
vec41i111 a b c d =
    Glsl.unsafeCall4 "vec4" [] a b c d


vec41i11f :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Vec4
vec41i11f a b c d =
    Glsl.unsafeCall4 "vec4" [] a b c (Glsl.float1 d)


vec41i11i1 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec4
vec41i11i1 a b c d =
    Glsl.unsafeCall4 "vec4" [] a b c d


vec41i11wi1 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Float_
    -> Int
    -> Glsl.Expression Glsl.Vec4
vec41i11wi1 a b c d =
    Glsl.unsafeCall4 "vec4" [] a b c (Glsl.int1 d)


vec41i1f1 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Int_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
vec41i1f1 a b c d =
    Glsl.unsafeCall4 "vec4" [] a b (Glsl.float1 c) d


vec41i1ff :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Int_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Vec4
vec41i1ff a b c d =
    Glsl.unsafeCall4 "vec4" [] a b (Glsl.float1 c) (Glsl.float1 d)


vec41i1fi1 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Int_
    -> Float
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec4
vec41i1fi1 a b c d =
    Glsl.unsafeCall4 "vec4" [] a b (Glsl.float1 c) d


vec41i1fwi1 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Int_
    -> Float
    -> Int
    -> Glsl.Expression Glsl.Vec4
vec41i1fwi1 a b c d =
    Glsl.unsafeCall4 "vec4" [] a b (Glsl.float1 c) (Glsl.int1 d)


vec41i1i11 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
vec41i1i11 a b c d =
    Glsl.unsafeCall4 "vec4" [] a b c d


vec41i1i1f :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Float
    -> Glsl.Expression Glsl.Vec4
vec41i1i1f a b c d =
    Glsl.unsafeCall4 "vec4" [] a b c (Glsl.float1 d)


vec41i1i1i1 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec4
vec41i1i1i1 a b c d =
    Glsl.unsafeCall4 "vec4" [] a b c d


vec41i1i1wi1 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Int
    -> Glsl.Expression Glsl.Vec4
vec41i1i1wi1 a b c d =
    Glsl.unsafeCall4 "vec4" [] a b c (Glsl.int1 d)


vec41i1wi11 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Int_
    -> Int
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
vec41i1wi11 a b c d =
    Glsl.unsafeCall4 "vec4" [] a b (Glsl.int1 c) d


vec41i1wi1f :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Int_
    -> Int
    -> Float
    -> Glsl.Expression Glsl.Vec4
vec41i1wi1f a b c d =
    Glsl.unsafeCall4 "vec4" [] a b (Glsl.int1 c) (Glsl.float1 d)


vec41i1wi1i1 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Int_
    -> Int
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec4
vec41i1wi1i1 a b c d =
    Glsl.unsafeCall4 "vec4" [] a b (Glsl.int1 c) d


vec41i1wi1wi1 :
    Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Int_
    -> Int
    -> Int
    -> Glsl.Expression Glsl.Vec4
vec41i1wi1wi1 a b c d =
    Glsl.unsafeCall4 "vec4" [] a b (Glsl.int1 c) (Glsl.int1 d)


vec41wi111 :
    Glsl.Expression Glsl.Float_
    -> Int
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
vec41wi111 a b c d =
    Glsl.unsafeCall4 "vec4" [] a (Glsl.int1 b) c d


vec41wi11f :
    Glsl.Expression Glsl.Float_
    -> Int
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Vec4
vec41wi11f a b c d =
    Glsl.unsafeCall4 "vec4" [] a (Glsl.int1 b) c (Glsl.float1 d)


vec41wi11i1 :
    Glsl.Expression Glsl.Float_
    -> Int
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec4
vec41wi11i1 a b c d =
    Glsl.unsafeCall4 "vec4" [] a (Glsl.int1 b) c d


vec41wi11wi1 :
    Glsl.Expression Glsl.Float_
    -> Int
    -> Glsl.Expression Glsl.Float_
    -> Int
    -> Glsl.Expression Glsl.Vec4
vec41wi11wi1 a b c d =
    Glsl.unsafeCall4 "vec4" [] a (Glsl.int1 b) c (Glsl.int1 d)


vec41wi1f1 :
    Glsl.Expression Glsl.Float_
    -> Int
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
vec41wi1f1 a b c d =
    Glsl.unsafeCall4 "vec4" [] a (Glsl.int1 b) (Glsl.float1 c) d


vec41wi1ff :
    Glsl.Expression Glsl.Float_
    -> Int
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Vec4
vec41wi1ff a b c d =
    Glsl.unsafeCall4 "vec4" [] a (Glsl.int1 b) (Glsl.float1 c) (Glsl.float1 d)


vec41wi1fi1 :
    Glsl.Expression Glsl.Float_
    -> Int
    -> Float
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec4
vec41wi1fi1 a b c d =
    Glsl.unsafeCall4 "vec4" [] a (Glsl.int1 b) (Glsl.float1 c) d


vec41wi1fwi1 :
    Glsl.Expression Glsl.Float_
    -> Int
    -> Float
    -> Int
    -> Glsl.Expression Glsl.Vec4
vec41wi1fwi1 a b c d =
    Glsl.unsafeCall4 "vec4" [] a (Glsl.int1 b) (Glsl.float1 c) (Glsl.int1 d)


vec41wi1i11 :
    Glsl.Expression Glsl.Float_
    -> Int
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
vec41wi1i11 a b c d =
    Glsl.unsafeCall4 "vec4" [] a (Glsl.int1 b) c d


vec41wi1i1f :
    Glsl.Expression Glsl.Float_
    -> Int
    -> Glsl.Expression Glsl.Int_
    -> Float
    -> Glsl.Expression Glsl.Vec4
vec41wi1i1f a b c d =
    Glsl.unsafeCall4 "vec4" [] a (Glsl.int1 b) c (Glsl.float1 d)


vec41wi1i1i1 :
    Glsl.Expression Glsl.Float_
    -> Int
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec4
vec41wi1i1i1 a b c d =
    Glsl.unsafeCall4 "vec4" [] a (Glsl.int1 b) c d


vec41wi1i1wi1 :
    Glsl.Expression Glsl.Float_
    -> Int
    -> Glsl.Expression Glsl.Int_
    -> Int
    -> Glsl.Expression Glsl.Vec4
vec41wi1i1wi1 a b c d =
    Glsl.unsafeCall4 "vec4" [] a (Glsl.int1 b) c (Glsl.int1 d)


vec41wi1wi11 :
    Glsl.Expression Glsl.Float_
    -> Int
    -> Int
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
vec41wi1wi11 a b c d =
    Glsl.unsafeCall4 "vec4" [] a (Glsl.int1 b) (Glsl.int1 c) d


vec41wi1wi1f :
    Glsl.Expression Glsl.Float_
    -> Int
    -> Int
    -> Float
    -> Glsl.Expression Glsl.Vec4
vec41wi1wi1f a b c d =
    Glsl.unsafeCall4 "vec4" [] a (Glsl.int1 b) (Glsl.int1 c) (Glsl.float1 d)


vec41wi1wi1i1 :
    Glsl.Expression Glsl.Float_
    -> Int
    -> Int
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec4
vec41wi1wi1i1 a b c d =
    Glsl.unsafeCall4 "vec4" [] a (Glsl.int1 b) (Glsl.int1 c) d


vec41wi1wi1wi1 :
    Glsl.Expression Glsl.Float_
    -> Int
    -> Int
    -> Int
    -> Glsl.Expression Glsl.Vec4
vec41wi1wi1wi1 a b c d =
    Glsl.unsafeCall4 "vec4" [] a (Glsl.int1 b) (Glsl.int1 c) (Glsl.int1 d)


vec422 :
    Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec2
    -> Glsl.Expression Glsl.Vec4
vec422 a b =
    Glsl.unsafeCall2 "vec4" [] a b


vec431 :
    Glsl.Expression Glsl.Vec3
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
vec431 a b =
    Glsl.unsafeCall2 "vec4" [] a b


vec43f : Glsl.Expression Glsl.Vec3 -> Float -> Glsl.Expression Glsl.Vec4
vec43f a b =
    Glsl.unsafeCall2 "vec4" [] a (Glsl.float1 b)


vec4f : Float -> Glsl.Expression Glsl.Vec4
vec4f a =
    Glsl.unsafeCall1 "vec4" [] (Glsl.float1 a)


vec4f111 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
vec4f111 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.float1 a) b c d


vec4f11f :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Vec4
vec4f11f a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.float1 a) b c (Glsl.float1 d)


vec4f11i1 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec4
vec4f11i1 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.float1 a) b c d


vec4f11wi1 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Int
    -> Glsl.Expression Glsl.Vec4
vec4f11wi1 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.float1 a) b c (Glsl.int1 d)


vec4f1f1 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
vec4f1f1 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.float1 a) b (Glsl.float1 c) d


vec4f1ff :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Vec4
vec4f1ff a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.float1 a) b (Glsl.float1 c) (Glsl.float1 d)


vec4f1fi1 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec4
vec4f1fi1 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.float1 a) b (Glsl.float1 c) d


vec4f1fwi1 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Int
    -> Glsl.Expression Glsl.Vec4
vec4f1fwi1 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.float1 a) b (Glsl.float1 c) (Glsl.int1 d)


vec4f1i11 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
vec4f1i11 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.float1 a) b c d


vec4f1i1f :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Int_
    -> Float
    -> Glsl.Expression Glsl.Vec4
vec4f1i1f a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.float1 a) b c (Glsl.float1 d)


vec4f1i1i1 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec4
vec4f1i1i1 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.float1 a) b c d


vec4f1i1wi1 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Int_
    -> Int
    -> Glsl.Expression Glsl.Vec4
vec4f1i1wi1 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.float1 a) b c (Glsl.int1 d)


vec4f1wi11 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Int
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
vec4f1wi11 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.float1 a) b (Glsl.int1 c) d


vec4f1wi1f :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Int
    -> Float
    -> Glsl.Expression Glsl.Vec4
vec4f1wi1f a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.float1 a) b (Glsl.int1 c) (Glsl.float1 d)


vec4f1wi1i1 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Int
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec4
vec4f1wi1i1 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.float1 a) b (Glsl.int1 c) d


vec4f1wi1wi1 :
    Float
    -> Glsl.Expression Glsl.Float_
    -> Int
    -> Int
    -> Glsl.Expression Glsl.Vec4
vec4f1wi1wi1 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.float1 a) b (Glsl.int1 c) (Glsl.int1 d)


vec4f3 : Float -> Glsl.Expression Glsl.Vec3 -> Glsl.Expression Glsl.Vec4
vec4f3 a b =
    Glsl.unsafeCall2 "vec4" [] (Glsl.float1 a) b


vec4ff11 :
    Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
vec4ff11 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.float1 a) (Glsl.float1 b) c d


vec4ff1f :
    Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Vec4
vec4ff1f a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.float1 a) (Glsl.float1 b) c (Glsl.float1 d)


vec4ff1i1 :
    Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec4
vec4ff1i1 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.float1 a) (Glsl.float1 b) c d


vec4ff1wi1 :
    Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Int
    -> Glsl.Expression Glsl.Vec4
vec4ff1wi1 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.float1 a) (Glsl.float1 b) c (Glsl.int1 d)


vec4fff1 :
    Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
vec4fff1 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.float1 a) (Glsl.float1 b) (Glsl.float1 c) d


vec4ffff : Float -> Float -> Float -> Float -> Glsl.Expression Glsl.Vec4
vec4ffff a b c d =
    Glsl.unsafeCall4
        "vec4"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        (Glsl.float1 c)
        (Glsl.float1 d)


vec4fffi1 :
    Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec4
vec4fffi1 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.float1 a) (Glsl.float1 b) (Glsl.float1 c) d


vec4fffwi1 : Float -> Float -> Float -> Int -> Glsl.Expression Glsl.Vec4
vec4fffwi1 a b c d =
    Glsl.unsafeCall4
        "vec4"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        (Glsl.float1 c)
        (Glsl.int1 d)


vec4ffi11 :
    Float
    -> Float
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
vec4ffi11 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.float1 a) (Glsl.float1 b) c d


vec4ffi1f :
    Float
    -> Float
    -> Glsl.Expression Glsl.Int_
    -> Float
    -> Glsl.Expression Glsl.Vec4
vec4ffi1f a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.float1 a) (Glsl.float1 b) c (Glsl.float1 d)


vec4ffi1i1 :
    Float
    -> Float
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec4
vec4ffi1i1 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.float1 a) (Glsl.float1 b) c d


vec4ffi1wi1 :
    Float
    -> Float
    -> Glsl.Expression Glsl.Int_
    -> Int
    -> Glsl.Expression Glsl.Vec4
vec4ffi1wi1 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.float1 a) (Glsl.float1 b) c (Glsl.int1 d)


vec4ffwi11 :
    Float
    -> Float
    -> Int
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
vec4ffwi11 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.float1 a) (Glsl.float1 b) (Glsl.int1 c) d


vec4ffwi1f : Float -> Float -> Int -> Float -> Glsl.Expression Glsl.Vec4
vec4ffwi1f a b c d =
    Glsl.unsafeCall4
        "vec4"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        (Glsl.int1 c)
        (Glsl.float1 d)


vec4ffwi1i1 :
    Float
    -> Float
    -> Int
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec4
vec4ffwi1i1 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.float1 a) (Glsl.float1 b) (Glsl.int1 c) d


vec4ffwi1wi1 : Float -> Float -> Int -> Int -> Glsl.Expression Glsl.Vec4
vec4ffwi1wi1 a b c d =
    Glsl.unsafeCall4
        "vec4"
        []
        (Glsl.float1 a)
        (Glsl.float1 b)
        (Glsl.int1 c)
        (Glsl.int1 d)


vec4fi111 :
    Float
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
vec4fi111 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.float1 a) b c d


vec4fi11f :
    Float
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Vec4
vec4fi11f a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.float1 a) b c (Glsl.float1 d)


vec4fi11i1 :
    Float
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec4
vec4fi11i1 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.float1 a) b c d


vec4fi11wi1 :
    Float
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Float_
    -> Int
    -> Glsl.Expression Glsl.Vec4
vec4fi11wi1 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.float1 a) b c (Glsl.int1 d)


vec4fi1f1 :
    Float
    -> Glsl.Expression Glsl.Int_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
vec4fi1f1 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.float1 a) b (Glsl.float1 c) d


vec4fi1ff :
    Float
    -> Glsl.Expression Glsl.Int_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Vec4
vec4fi1ff a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.float1 a) b (Glsl.float1 c) (Glsl.float1 d)


vec4fi1fi1 :
    Float
    -> Glsl.Expression Glsl.Int_
    -> Float
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec4
vec4fi1fi1 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.float1 a) b (Glsl.float1 c) d


vec4fi1fwi1 :
    Float
    -> Glsl.Expression Glsl.Int_
    -> Float
    -> Int
    -> Glsl.Expression Glsl.Vec4
vec4fi1fwi1 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.float1 a) b (Glsl.float1 c) (Glsl.int1 d)


vec4fi1i11 :
    Float
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
vec4fi1i11 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.float1 a) b c d


vec4fi1i1f :
    Float
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Float
    -> Glsl.Expression Glsl.Vec4
vec4fi1i1f a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.float1 a) b c (Glsl.float1 d)


vec4fi1i1i1 :
    Float
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec4
vec4fi1i1i1 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.float1 a) b c d


vec4fi1i1wi1 :
    Float
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Int
    -> Glsl.Expression Glsl.Vec4
vec4fi1i1wi1 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.float1 a) b c (Glsl.int1 d)


vec4fi1wi11 :
    Float
    -> Glsl.Expression Glsl.Int_
    -> Int
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
vec4fi1wi11 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.float1 a) b (Glsl.int1 c) d


vec4fi1wi1f :
    Float
    -> Glsl.Expression Glsl.Int_
    -> Int
    -> Float
    -> Glsl.Expression Glsl.Vec4
vec4fi1wi1f a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.float1 a) b (Glsl.int1 c) (Glsl.float1 d)


vec4fi1wi1i1 :
    Float
    -> Glsl.Expression Glsl.Int_
    -> Int
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec4
vec4fi1wi1i1 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.float1 a) b (Glsl.int1 c) d


vec4fi1wi1wi1 :
    Float
    -> Glsl.Expression Glsl.Int_
    -> Int
    -> Int
    -> Glsl.Expression Glsl.Vec4
vec4fi1wi1wi1 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.float1 a) b (Glsl.int1 c) (Glsl.int1 d)


vec4fwi111 :
    Float
    -> Int
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
vec4fwi111 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.float1 a) (Glsl.int1 b) c d


vec4fwi11f :
    Float
    -> Int
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Vec4
vec4fwi11f a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.float1 a) (Glsl.int1 b) c (Glsl.float1 d)


vec4fwi11i1 :
    Float
    -> Int
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec4
vec4fwi11i1 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.float1 a) (Glsl.int1 b) c d


vec4fwi11wi1 :
    Float
    -> Int
    -> Glsl.Expression Glsl.Float_
    -> Int
    -> Glsl.Expression Glsl.Vec4
vec4fwi11wi1 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.float1 a) (Glsl.int1 b) c (Glsl.int1 d)


vec4fwi1f1 :
    Float
    -> Int
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
vec4fwi1f1 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.float1 a) (Glsl.int1 b) (Glsl.float1 c) d


vec4fwi1ff : Float -> Int -> Float -> Float -> Glsl.Expression Glsl.Vec4
vec4fwi1ff a b c d =
    Glsl.unsafeCall4
        "vec4"
        []
        (Glsl.float1 a)
        (Glsl.int1 b)
        (Glsl.float1 c)
        (Glsl.float1 d)


vec4fwi1fi1 :
    Float
    -> Int
    -> Float
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec4
vec4fwi1fi1 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.float1 a) (Glsl.int1 b) (Glsl.float1 c) d


vec4fwi1fwi1 : Float -> Int -> Float -> Int -> Glsl.Expression Glsl.Vec4
vec4fwi1fwi1 a b c d =
    Glsl.unsafeCall4
        "vec4"
        []
        (Glsl.float1 a)
        (Glsl.int1 b)
        (Glsl.float1 c)
        (Glsl.int1 d)


vec4fwi1i11 :
    Float
    -> Int
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
vec4fwi1i11 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.float1 a) (Glsl.int1 b) c d


vec4fwi1i1f :
    Float
    -> Int
    -> Glsl.Expression Glsl.Int_
    -> Float
    -> Glsl.Expression Glsl.Vec4
vec4fwi1i1f a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.float1 a) (Glsl.int1 b) c (Glsl.float1 d)


vec4fwi1i1i1 :
    Float
    -> Int
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec4
vec4fwi1i1i1 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.float1 a) (Glsl.int1 b) c d


vec4fwi1i1wi1 :
    Float
    -> Int
    -> Glsl.Expression Glsl.Int_
    -> Int
    -> Glsl.Expression Glsl.Vec4
vec4fwi1i1wi1 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.float1 a) (Glsl.int1 b) c (Glsl.int1 d)


vec4fwi1wi11 :
    Float
    -> Int
    -> Int
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
vec4fwi1wi11 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.float1 a) (Glsl.int1 b) (Glsl.int1 c) d


vec4fwi1wi1f : Float -> Int -> Int -> Float -> Glsl.Expression Glsl.Vec4
vec4fwi1wi1f a b c d =
    Glsl.unsafeCall4
        "vec4"
        []
        (Glsl.float1 a)
        (Glsl.int1 b)
        (Glsl.int1 c)
        (Glsl.float1 d)


vec4fwi1wi1i1 :
    Float
    -> Int
    -> Int
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec4
vec4fwi1wi1i1 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.float1 a) (Glsl.int1 b) (Glsl.int1 c) d


vec4fwi1wi1wi1 : Float -> Int -> Int -> Int -> Glsl.Expression Glsl.Vec4
vec4fwi1wi1wi1 a b c d =
    Glsl.unsafeCall4
        "vec4"
        []
        (Glsl.float1 a)
        (Glsl.int1 b)
        (Glsl.int1 c)
        (Glsl.int1 d)


vec4i1 : Glsl.Expression Glsl.Int_ -> Glsl.Expression Glsl.Vec4
vec4i1 a =
    Glsl.unsafeCall1 "vec4" [] a


vec4i1111 :
    Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
vec4i1111 a b c d =
    Glsl.unsafeCall4 "vec4" [] a b c d


vec4i111f :
    Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Vec4
vec4i111f a b c d =
    Glsl.unsafeCall4 "vec4" [] a b c (Glsl.float1 d)


vec4i111i1 :
    Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec4
vec4i111i1 a b c d =
    Glsl.unsafeCall4 "vec4" [] a b c d


vec4i111wi1 :
    Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Int
    -> Glsl.Expression Glsl.Vec4
vec4i111wi1 a b c d =
    Glsl.unsafeCall4 "vec4" [] a b c (Glsl.int1 d)


vec4i11f1 :
    Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
vec4i11f1 a b c d =
    Glsl.unsafeCall4 "vec4" [] a b (Glsl.float1 c) d


vec4i11ff :
    Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Vec4
vec4i11ff a b c d =
    Glsl.unsafeCall4 "vec4" [] a b (Glsl.float1 c) (Glsl.float1 d)


vec4i11fi1 :
    Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec4
vec4i11fi1 a b c d =
    Glsl.unsafeCall4 "vec4" [] a b (Glsl.float1 c) d


vec4i11fwi1 :
    Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Int
    -> Glsl.Expression Glsl.Vec4
vec4i11fwi1 a b c d =
    Glsl.unsafeCall4 "vec4" [] a b (Glsl.float1 c) (Glsl.int1 d)


vec4i11i11 :
    Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
vec4i11i11 a b c d =
    Glsl.unsafeCall4 "vec4" [] a b c d


vec4i11i1f :
    Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Int_
    -> Float
    -> Glsl.Expression Glsl.Vec4
vec4i11i1f a b c d =
    Glsl.unsafeCall4 "vec4" [] a b c (Glsl.float1 d)


vec4i11i1i1 :
    Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec4
vec4i11i1i1 a b c d =
    Glsl.unsafeCall4 "vec4" [] a b c d


vec4i11i1wi1 :
    Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Int_
    -> Int
    -> Glsl.Expression Glsl.Vec4
vec4i11i1wi1 a b c d =
    Glsl.unsafeCall4 "vec4" [] a b c (Glsl.int1 d)


vec4i11wi11 :
    Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Float_
    -> Int
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
vec4i11wi11 a b c d =
    Glsl.unsafeCall4 "vec4" [] a b (Glsl.int1 c) d


vec4i11wi1f :
    Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Float_
    -> Int
    -> Float
    -> Glsl.Expression Glsl.Vec4
vec4i11wi1f a b c d =
    Glsl.unsafeCall4 "vec4" [] a b (Glsl.int1 c) (Glsl.float1 d)


vec4i11wi1i1 :
    Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Float_
    -> Int
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec4
vec4i11wi1i1 a b c d =
    Glsl.unsafeCall4 "vec4" [] a b (Glsl.int1 c) d


vec4i11wi1wi1 :
    Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Float_
    -> Int
    -> Int
    -> Glsl.Expression Glsl.Vec4
vec4i11wi1wi1 a b c d =
    Glsl.unsafeCall4 "vec4" [] a b (Glsl.int1 c) (Glsl.int1 d)


vec4i1f11 :
    Glsl.Expression Glsl.Int_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
vec4i1f11 a b c d =
    Glsl.unsafeCall4 "vec4" [] a (Glsl.float1 b) c d


vec4i1f1f :
    Glsl.Expression Glsl.Int_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Vec4
vec4i1f1f a b c d =
    Glsl.unsafeCall4 "vec4" [] a (Glsl.float1 b) c (Glsl.float1 d)


vec4i1f1i1 :
    Glsl.Expression Glsl.Int_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec4
vec4i1f1i1 a b c d =
    Glsl.unsafeCall4 "vec4" [] a (Glsl.float1 b) c d


vec4i1f1wi1 :
    Glsl.Expression Glsl.Int_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Int
    -> Glsl.Expression Glsl.Vec4
vec4i1f1wi1 a b c d =
    Glsl.unsafeCall4 "vec4" [] a (Glsl.float1 b) c (Glsl.int1 d)


vec4i1ff1 :
    Glsl.Expression Glsl.Int_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
vec4i1ff1 a b c d =
    Glsl.unsafeCall4 "vec4" [] a (Glsl.float1 b) (Glsl.float1 c) d


vec4i1fff :
    Glsl.Expression Glsl.Int_
    -> Float
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Vec4
vec4i1fff a b c d =
    Glsl.unsafeCall4 "vec4" [] a (Glsl.float1 b) (Glsl.float1 c) (Glsl.float1 d)


vec4i1ffi1 :
    Glsl.Expression Glsl.Int_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec4
vec4i1ffi1 a b c d =
    Glsl.unsafeCall4 "vec4" [] a (Glsl.float1 b) (Glsl.float1 c) d


vec4i1ffwi1 :
    Glsl.Expression Glsl.Int_
    -> Float
    -> Float
    -> Int
    -> Glsl.Expression Glsl.Vec4
vec4i1ffwi1 a b c d =
    Glsl.unsafeCall4 "vec4" [] a (Glsl.float1 b) (Glsl.float1 c) (Glsl.int1 d)


vec4i1fi11 :
    Glsl.Expression Glsl.Int_
    -> Float
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
vec4i1fi11 a b c d =
    Glsl.unsafeCall4 "vec4" [] a (Glsl.float1 b) c d


vec4i1fi1f :
    Glsl.Expression Glsl.Int_
    -> Float
    -> Glsl.Expression Glsl.Int_
    -> Float
    -> Glsl.Expression Glsl.Vec4
vec4i1fi1f a b c d =
    Glsl.unsafeCall4 "vec4" [] a (Glsl.float1 b) c (Glsl.float1 d)


vec4i1fi1i1 :
    Glsl.Expression Glsl.Int_
    -> Float
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec4
vec4i1fi1i1 a b c d =
    Glsl.unsafeCall4 "vec4" [] a (Glsl.float1 b) c d


vec4i1fi1wi1 :
    Glsl.Expression Glsl.Int_
    -> Float
    -> Glsl.Expression Glsl.Int_
    -> Int
    -> Glsl.Expression Glsl.Vec4
vec4i1fi1wi1 a b c d =
    Glsl.unsafeCall4 "vec4" [] a (Glsl.float1 b) c (Glsl.int1 d)


vec4i1fwi11 :
    Glsl.Expression Glsl.Int_
    -> Float
    -> Int
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
vec4i1fwi11 a b c d =
    Glsl.unsafeCall4 "vec4" [] a (Glsl.float1 b) (Glsl.int1 c) d


vec4i1fwi1f :
    Glsl.Expression Glsl.Int_
    -> Float
    -> Int
    -> Float
    -> Glsl.Expression Glsl.Vec4
vec4i1fwi1f a b c d =
    Glsl.unsafeCall4 "vec4" [] a (Glsl.float1 b) (Glsl.int1 c) (Glsl.float1 d)


vec4i1fwi1i1 :
    Glsl.Expression Glsl.Int_
    -> Float
    -> Int
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec4
vec4i1fwi1i1 a b c d =
    Glsl.unsafeCall4 "vec4" [] a (Glsl.float1 b) (Glsl.int1 c) d


vec4i1fwi1wi1 :
    Glsl.Expression Glsl.Int_
    -> Float
    -> Int
    -> Int
    -> Glsl.Expression Glsl.Vec4
vec4i1fwi1wi1 a b c d =
    Glsl.unsafeCall4 "vec4" [] a (Glsl.float1 b) (Glsl.int1 c) (Glsl.int1 d)


vec4i1i111 :
    Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
vec4i1i111 a b c d =
    Glsl.unsafeCall4 "vec4" [] a b c d


vec4i1i11f :
    Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Vec4
vec4i1i11f a b c d =
    Glsl.unsafeCall4 "vec4" [] a b c (Glsl.float1 d)


vec4i1i11i1 :
    Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec4
vec4i1i11i1 a b c d =
    Glsl.unsafeCall4 "vec4" [] a b c d


vec4i1i11wi1 :
    Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Float_
    -> Int
    -> Glsl.Expression Glsl.Vec4
vec4i1i11wi1 a b c d =
    Glsl.unsafeCall4 "vec4" [] a b c (Glsl.int1 d)


vec4i1i1f1 :
    Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
vec4i1i1f1 a b c d =
    Glsl.unsafeCall4 "vec4" [] a b (Glsl.float1 c) d


vec4i1i1ff :
    Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Vec4
vec4i1i1ff a b c d =
    Glsl.unsafeCall4 "vec4" [] a b (Glsl.float1 c) (Glsl.float1 d)


vec4i1i1fi1 :
    Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Float
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec4
vec4i1i1fi1 a b c d =
    Glsl.unsafeCall4 "vec4" [] a b (Glsl.float1 c) d


vec4i1i1fwi1 :
    Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Float
    -> Int
    -> Glsl.Expression Glsl.Vec4
vec4i1i1fwi1 a b c d =
    Glsl.unsafeCall4 "vec4" [] a b (Glsl.float1 c) (Glsl.int1 d)


vec4i1i1i11 :
    Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
vec4i1i1i11 a b c d =
    Glsl.unsafeCall4 "vec4" [] a b c d


vec4i1i1i1f :
    Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Float
    -> Glsl.Expression Glsl.Vec4
vec4i1i1i1f a b c d =
    Glsl.unsafeCall4 "vec4" [] a b c (Glsl.float1 d)


vec4i1i1i1i1 :
    Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec4
vec4i1i1i1i1 a b c d =
    Glsl.unsafeCall4 "vec4" [] a b c d


vec4i1i1i1wi1 :
    Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Int
    -> Glsl.Expression Glsl.Vec4
vec4i1i1i1wi1 a b c d =
    Glsl.unsafeCall4 "vec4" [] a b c (Glsl.int1 d)


vec4i1i1wi11 :
    Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Int
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
vec4i1i1wi11 a b c d =
    Glsl.unsafeCall4 "vec4" [] a b (Glsl.int1 c) d


vec4i1i1wi1f :
    Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Int
    -> Float
    -> Glsl.Expression Glsl.Vec4
vec4i1i1wi1f a b c d =
    Glsl.unsafeCall4 "vec4" [] a b (Glsl.int1 c) (Glsl.float1 d)


vec4i1i1wi1i1 :
    Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Int
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec4
vec4i1i1wi1i1 a b c d =
    Glsl.unsafeCall4 "vec4" [] a b (Glsl.int1 c) d


vec4i1i1wi1wi1 :
    Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Int
    -> Int
    -> Glsl.Expression Glsl.Vec4
vec4i1i1wi1wi1 a b c d =
    Glsl.unsafeCall4 "vec4" [] a b (Glsl.int1 c) (Glsl.int1 d)


vec4i1wi111 :
    Glsl.Expression Glsl.Int_
    -> Int
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
vec4i1wi111 a b c d =
    Glsl.unsafeCall4 "vec4" [] a (Glsl.int1 b) c d


vec4i1wi11f :
    Glsl.Expression Glsl.Int_
    -> Int
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Vec4
vec4i1wi11f a b c d =
    Glsl.unsafeCall4 "vec4" [] a (Glsl.int1 b) c (Glsl.float1 d)


vec4i1wi11i1 :
    Glsl.Expression Glsl.Int_
    -> Int
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec4
vec4i1wi11i1 a b c d =
    Glsl.unsafeCall4 "vec4" [] a (Glsl.int1 b) c d


vec4i1wi11wi1 :
    Glsl.Expression Glsl.Int_
    -> Int
    -> Glsl.Expression Glsl.Float_
    -> Int
    -> Glsl.Expression Glsl.Vec4
vec4i1wi11wi1 a b c d =
    Glsl.unsafeCall4 "vec4" [] a (Glsl.int1 b) c (Glsl.int1 d)


vec4i1wi1f1 :
    Glsl.Expression Glsl.Int_
    -> Int
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
vec4i1wi1f1 a b c d =
    Glsl.unsafeCall4 "vec4" [] a (Glsl.int1 b) (Glsl.float1 c) d


vec4i1wi1ff :
    Glsl.Expression Glsl.Int_
    -> Int
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Vec4
vec4i1wi1ff a b c d =
    Glsl.unsafeCall4 "vec4" [] a (Glsl.int1 b) (Glsl.float1 c) (Glsl.float1 d)


vec4i1wi1fi1 :
    Glsl.Expression Glsl.Int_
    -> Int
    -> Float
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec4
vec4i1wi1fi1 a b c d =
    Glsl.unsafeCall4 "vec4" [] a (Glsl.int1 b) (Glsl.float1 c) d


vec4i1wi1fwi1 :
    Glsl.Expression Glsl.Int_
    -> Int
    -> Float
    -> Int
    -> Glsl.Expression Glsl.Vec4
vec4i1wi1fwi1 a b c d =
    Glsl.unsafeCall4 "vec4" [] a (Glsl.int1 b) (Glsl.float1 c) (Glsl.int1 d)


vec4i1wi1i11 :
    Glsl.Expression Glsl.Int_
    -> Int
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
vec4i1wi1i11 a b c d =
    Glsl.unsafeCall4 "vec4" [] a (Glsl.int1 b) c d


vec4i1wi1i1f :
    Glsl.Expression Glsl.Int_
    -> Int
    -> Glsl.Expression Glsl.Int_
    -> Float
    -> Glsl.Expression Glsl.Vec4
vec4i1wi1i1f a b c d =
    Glsl.unsafeCall4 "vec4" [] a (Glsl.int1 b) c (Glsl.float1 d)


vec4i1wi1i1i1 :
    Glsl.Expression Glsl.Int_
    -> Int
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec4
vec4i1wi1i1i1 a b c d =
    Glsl.unsafeCall4 "vec4" [] a (Glsl.int1 b) c d


vec4i1wi1i1wi1 :
    Glsl.Expression Glsl.Int_
    -> Int
    -> Glsl.Expression Glsl.Int_
    -> Int
    -> Glsl.Expression Glsl.Vec4
vec4i1wi1i1wi1 a b c d =
    Glsl.unsafeCall4 "vec4" [] a (Glsl.int1 b) c (Glsl.int1 d)


vec4i1wi1wi11 :
    Glsl.Expression Glsl.Int_
    -> Int
    -> Int
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
vec4i1wi1wi11 a b c d =
    Glsl.unsafeCall4 "vec4" [] a (Glsl.int1 b) (Glsl.int1 c) d


vec4i1wi1wi1f :
    Glsl.Expression Glsl.Int_
    -> Int
    -> Int
    -> Float
    -> Glsl.Expression Glsl.Vec4
vec4i1wi1wi1f a b c d =
    Glsl.unsafeCall4 "vec4" [] a (Glsl.int1 b) (Glsl.int1 c) (Glsl.float1 d)


vec4i1wi1wi1i1 :
    Glsl.Expression Glsl.Int_
    -> Int
    -> Int
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec4
vec4i1wi1wi1i1 a b c d =
    Glsl.unsafeCall4 "vec4" [] a (Glsl.int1 b) (Glsl.int1 c) d


vec4i1wi1wi1wi1 : Glsl.Expression Glsl.Int_ -> Int -> Int -> Int -> Glsl.Expression Glsl.Vec4
vec4i1wi1wi1wi1 a b c d =
    Glsl.unsafeCall4 "vec4" [] a (Glsl.int1 b) (Glsl.int1 c) (Glsl.int1 d)


vec4wi1 : Int -> Glsl.Expression Glsl.Vec4
vec4wi1 a =
    Glsl.unsafeCall1 "vec4" [] (Glsl.int1 a)


vec4wi1111 :
    Int
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
vec4wi1111 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.int1 a) b c d


vec4wi111f :
    Int
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Vec4
vec4wi111f a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.int1 a) b c (Glsl.float1 d)


vec4wi111i1 :
    Int
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec4
vec4wi111i1 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.int1 a) b c d


vec4wi111wi1 :
    Int
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Int
    -> Glsl.Expression Glsl.Vec4
vec4wi111wi1 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.int1 a) b c (Glsl.int1 d)


vec4wi11f1 :
    Int
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
vec4wi11f1 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.int1 a) b (Glsl.float1 c) d


vec4wi11ff :
    Int
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Vec4
vec4wi11ff a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.int1 a) b (Glsl.float1 c) (Glsl.float1 d)


vec4wi11fi1 :
    Int
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec4
vec4wi11fi1 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.int1 a) b (Glsl.float1 c) d


vec4wi11fwi1 :
    Int
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Int
    -> Glsl.Expression Glsl.Vec4
vec4wi11fwi1 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.int1 a) b (Glsl.float1 c) (Glsl.int1 d)


vec4wi11i11 :
    Int
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
vec4wi11i11 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.int1 a) b c d


vec4wi11i1f :
    Int
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Int_
    -> Float
    -> Glsl.Expression Glsl.Vec4
vec4wi11i1f a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.int1 a) b c (Glsl.float1 d)


vec4wi11i1i1 :
    Int
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec4
vec4wi11i1i1 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.int1 a) b c d


vec4wi11i1wi1 :
    Int
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Int_
    -> Int
    -> Glsl.Expression Glsl.Vec4
vec4wi11i1wi1 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.int1 a) b c (Glsl.int1 d)


vec4wi11wi11 :
    Int
    -> Glsl.Expression Glsl.Float_
    -> Int
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
vec4wi11wi11 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.int1 a) b (Glsl.int1 c) d


vec4wi11wi1f :
    Int
    -> Glsl.Expression Glsl.Float_
    -> Int
    -> Float
    -> Glsl.Expression Glsl.Vec4
vec4wi11wi1f a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.int1 a) b (Glsl.int1 c) (Glsl.float1 d)


vec4wi11wi1i1 :
    Int
    -> Glsl.Expression Glsl.Float_
    -> Int
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec4
vec4wi11wi1i1 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.int1 a) b (Glsl.int1 c) d


vec4wi11wi1wi1 :
    Int
    -> Glsl.Expression Glsl.Float_
    -> Int
    -> Int
    -> Glsl.Expression Glsl.Vec4
vec4wi11wi1wi1 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.int1 a) b (Glsl.int1 c) (Glsl.int1 d)


vec4wi1f11 :
    Int
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
vec4wi1f11 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.int1 a) (Glsl.float1 b) c d


vec4wi1f1f :
    Int
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Vec4
vec4wi1f1f a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.int1 a) (Glsl.float1 b) c (Glsl.float1 d)


vec4wi1f1i1 :
    Int
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec4
vec4wi1f1i1 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.int1 a) (Glsl.float1 b) c d


vec4wi1f1wi1 :
    Int
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Int
    -> Glsl.Expression Glsl.Vec4
vec4wi1f1wi1 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.int1 a) (Glsl.float1 b) c (Glsl.int1 d)


vec4wi1ff1 :
    Int
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
vec4wi1ff1 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.int1 a) (Glsl.float1 b) (Glsl.float1 c) d


vec4wi1fff : Int -> Float -> Float -> Float -> Glsl.Expression Glsl.Vec4
vec4wi1fff a b c d =
    Glsl.unsafeCall4
        "vec4"
        []
        (Glsl.int1 a)
        (Glsl.float1 b)
        (Glsl.float1 c)
        (Glsl.float1 d)


vec4wi1ffi1 :
    Int
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec4
vec4wi1ffi1 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.int1 a) (Glsl.float1 b) (Glsl.float1 c) d


vec4wi1ffwi1 : Int -> Float -> Float -> Int -> Glsl.Expression Glsl.Vec4
vec4wi1ffwi1 a b c d =
    Glsl.unsafeCall4
        "vec4"
        []
        (Glsl.int1 a)
        (Glsl.float1 b)
        (Glsl.float1 c)
        (Glsl.int1 d)


vec4wi1fi11 :
    Int
    -> Float
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
vec4wi1fi11 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.int1 a) (Glsl.float1 b) c d


vec4wi1fi1f :
    Int
    -> Float
    -> Glsl.Expression Glsl.Int_
    -> Float
    -> Glsl.Expression Glsl.Vec4
vec4wi1fi1f a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.int1 a) (Glsl.float1 b) c (Glsl.float1 d)


vec4wi1fi1i1 :
    Int
    -> Float
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec4
vec4wi1fi1i1 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.int1 a) (Glsl.float1 b) c d


vec4wi1fi1wi1 :
    Int
    -> Float
    -> Glsl.Expression Glsl.Int_
    -> Int
    -> Glsl.Expression Glsl.Vec4
vec4wi1fi1wi1 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.int1 a) (Glsl.float1 b) c (Glsl.int1 d)


vec4wi1fwi11 :
    Int
    -> Float
    -> Int
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
vec4wi1fwi11 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.int1 a) (Glsl.float1 b) (Glsl.int1 c) d


vec4wi1fwi1f : Int -> Float -> Int -> Float -> Glsl.Expression Glsl.Vec4
vec4wi1fwi1f a b c d =
    Glsl.unsafeCall4
        "vec4"
        []
        (Glsl.int1 a)
        (Glsl.float1 b)
        (Glsl.int1 c)
        (Glsl.float1 d)


vec4wi1fwi1i1 :
    Int
    -> Float
    -> Int
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec4
vec4wi1fwi1i1 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.int1 a) (Glsl.float1 b) (Glsl.int1 c) d


vec4wi1fwi1wi1 : Int -> Float -> Int -> Int -> Glsl.Expression Glsl.Vec4
vec4wi1fwi1wi1 a b c d =
    Glsl.unsafeCall4
        "vec4"
        []
        (Glsl.int1 a)
        (Glsl.float1 b)
        (Glsl.int1 c)
        (Glsl.int1 d)


vec4wi1i111 :
    Int
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
vec4wi1i111 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.int1 a) b c d


vec4wi1i11f :
    Int
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Vec4
vec4wi1i11f a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.int1 a) b c (Glsl.float1 d)


vec4wi1i11i1 :
    Int
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec4
vec4wi1i11i1 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.int1 a) b c d


vec4wi1i11wi1 :
    Int
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Float_
    -> Int
    -> Glsl.Expression Glsl.Vec4
vec4wi1i11wi1 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.int1 a) b c (Glsl.int1 d)


vec4wi1i1f1 :
    Int
    -> Glsl.Expression Glsl.Int_
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
vec4wi1i1f1 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.int1 a) b (Glsl.float1 c) d


vec4wi1i1ff :
    Int
    -> Glsl.Expression Glsl.Int_
    -> Float
    -> Float
    -> Glsl.Expression Glsl.Vec4
vec4wi1i1ff a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.int1 a) b (Glsl.float1 c) (Glsl.float1 d)


vec4wi1i1fi1 :
    Int
    -> Glsl.Expression Glsl.Int_
    -> Float
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec4
vec4wi1i1fi1 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.int1 a) b (Glsl.float1 c) d


vec4wi1i1fwi1 :
    Int
    -> Glsl.Expression Glsl.Int_
    -> Float
    -> Int
    -> Glsl.Expression Glsl.Vec4
vec4wi1i1fwi1 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.int1 a) b (Glsl.float1 c) (Glsl.int1 d)


vec4wi1i1i11 :
    Int
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
vec4wi1i1i11 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.int1 a) b c d


vec4wi1i1i1f :
    Int
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Float
    -> Glsl.Expression Glsl.Vec4
vec4wi1i1i1f a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.int1 a) b c (Glsl.float1 d)


vec4wi1i1i1i1 :
    Int
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec4
vec4wi1i1i1i1 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.int1 a) b c d


vec4wi1i1i1wi1 :
    Int
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Int
    -> Glsl.Expression Glsl.Vec4
vec4wi1i1i1wi1 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.int1 a) b c (Glsl.int1 d)


vec4wi1i1wi11 :
    Int
    -> Glsl.Expression Glsl.Int_
    -> Int
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
vec4wi1i1wi11 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.int1 a) b (Glsl.int1 c) d


vec4wi1i1wi1f :
    Int
    -> Glsl.Expression Glsl.Int_
    -> Int
    -> Float
    -> Glsl.Expression Glsl.Vec4
vec4wi1i1wi1f a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.int1 a) b (Glsl.int1 c) (Glsl.float1 d)


vec4wi1i1wi1i1 :
    Int
    -> Glsl.Expression Glsl.Int_
    -> Int
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec4
vec4wi1i1wi1i1 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.int1 a) b (Glsl.int1 c) d


vec4wi1i1wi1wi1 : Int -> Glsl.Expression Glsl.Int_ -> Int -> Int -> Glsl.Expression Glsl.Vec4
vec4wi1i1wi1wi1 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.int1 a) b (Glsl.int1 c) (Glsl.int1 d)


vec4wi1wi111 :
    Int
    -> Int
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
vec4wi1wi111 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.int1 a) (Glsl.int1 b) c d


vec4wi1wi11f :
    Int
    -> Int
    -> Glsl.Expression Glsl.Float_
    -> Float
    -> Glsl.Expression Glsl.Vec4
vec4wi1wi11f a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.int1 a) (Glsl.int1 b) c (Glsl.float1 d)


vec4wi1wi11i1 :
    Int
    -> Int
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec4
vec4wi1wi11i1 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.int1 a) (Glsl.int1 b) c d


vec4wi1wi11wi1 :
    Int
    -> Int
    -> Glsl.Expression Glsl.Float_
    -> Int
    -> Glsl.Expression Glsl.Vec4
vec4wi1wi11wi1 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.int1 a) (Glsl.int1 b) c (Glsl.int1 d)


vec4wi1wi1f1 :
    Int
    -> Int
    -> Float
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
vec4wi1wi1f1 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.int1 a) (Glsl.int1 b) (Glsl.float1 c) d


vec4wi1wi1ff : Int -> Int -> Float -> Float -> Glsl.Expression Glsl.Vec4
vec4wi1wi1ff a b c d =
    Glsl.unsafeCall4
        "vec4"
        []
        (Glsl.int1 a)
        (Glsl.int1 b)
        (Glsl.float1 c)
        (Glsl.float1 d)


vec4wi1wi1fi1 :
    Int
    -> Int
    -> Float
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec4
vec4wi1wi1fi1 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.int1 a) (Glsl.int1 b) (Glsl.float1 c) d


vec4wi1wi1fwi1 : Int -> Int -> Float -> Int -> Glsl.Expression Glsl.Vec4
vec4wi1wi1fwi1 a b c d =
    Glsl.unsafeCall4
        "vec4"
        []
        (Glsl.int1 a)
        (Glsl.int1 b)
        (Glsl.float1 c)
        (Glsl.int1 d)


vec4wi1wi1i11 :
    Int
    -> Int
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
vec4wi1wi1i11 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.int1 a) (Glsl.int1 b) c d


vec4wi1wi1i1f :
    Int
    -> Int
    -> Glsl.Expression Glsl.Int_
    -> Float
    -> Glsl.Expression Glsl.Vec4
vec4wi1wi1i1f a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.int1 a) (Glsl.int1 b) c (Glsl.float1 d)


vec4wi1wi1i1i1 :
    Int
    -> Int
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Int_
    -> Glsl.Expression Glsl.Vec4
vec4wi1wi1i1i1 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.int1 a) (Glsl.int1 b) c d


vec4wi1wi1i1wi1 : Int -> Int -> Glsl.Expression Glsl.Int_ -> Int -> Glsl.Expression Glsl.Vec4
vec4wi1wi1i1wi1 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.int1 a) (Glsl.int1 b) c (Glsl.int1 d)


vec4wi1wi1wi11 :
    Int
    -> Int
    -> Int
    -> Glsl.Expression Glsl.Float_
    -> Glsl.Expression Glsl.Vec4
vec4wi1wi1wi11 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.int1 a) (Glsl.int1 b) (Glsl.int1 c) d


vec4wi1wi1wi1f : Int -> Int -> Int -> Float -> Glsl.Expression Glsl.Vec4
vec4wi1wi1wi1f a b c d =
    Glsl.unsafeCall4
        "vec4"
        []
        (Glsl.int1 a)
        (Glsl.int1 b)
        (Glsl.int1 c)
        (Glsl.float1 d)


vec4wi1wi1wi1i1 : Int -> Int -> Int -> Glsl.Expression Glsl.Int_ -> Glsl.Expression Glsl.Vec4
vec4wi1wi1wi1i1 a b c d =
    Glsl.unsafeCall4 "vec4" [] (Glsl.int1 a) (Glsl.int1 b) (Glsl.int1 c) d


vec4wi1wi1wi1wi1 : Int -> Int -> Int -> Int -> Glsl.Expression Glsl.Vec4
vec4wi1wi1wi1wi1 a b c d =
    Glsl.unsafeCall4
        "vec4"
        []
        (Glsl.int1 a)
        (Glsl.int1 b)
        (Glsl.int1 c)
        (Glsl.int1 d)


radians_ : Glsl.Expression (Glsl.Vec Float a) -> Glsl.Expression (Glsl.Vec Float a)
radians_ a =
    Glsl.unsafeCall1 "radians" [] a


degrees_ : Glsl.Expression (Glsl.Vec Float a) -> Glsl.Expression (Glsl.Vec Float a)
degrees_ a =
    Glsl.unsafeCall1 "degrees" [] a


sin_ : Glsl.Expression (Glsl.Vec Float a) -> Glsl.Expression (Glsl.Vec Float a)
sin_ a =
    Glsl.unsafeCall1 "sin" [] a


cos_ : Glsl.Expression (Glsl.Vec Float a) -> Glsl.Expression (Glsl.Vec Float a)
cos_ a =
    Glsl.unsafeCall1 "cos" [] a


tan_ : Glsl.Expression (Glsl.Vec Float a) -> Glsl.Expression (Glsl.Vec Float a)
tan_ a =
    Glsl.unsafeCall1 "tan" [] a


asin_ : Glsl.Expression (Glsl.Vec Float a) -> Glsl.Expression (Glsl.Vec Float a)
asin_ a =
    Glsl.unsafeCall1 "asin" [] a


acos_ : Glsl.Expression (Glsl.Vec Float a) -> Glsl.Expression (Glsl.Vec Float a)
acos_ a =
    Glsl.unsafeCall1 "acos" [] a


atan_ : Glsl.Expression (Glsl.Vec Float a) -> Glsl.Expression (Glsl.Vec Float a)
atan_ a =
    Glsl.unsafeCall1 "atan" [] a


atan2_ :
    Glsl.Expression (Glsl.Vec Float a)
    -> Glsl.Expression (Glsl.Vec Float a)
    -> Glsl.Expression (Glsl.Vec Float a)
atan2_ a b =
    Glsl.unsafeCall2 "atan" [] a b


sinh : Glsl.Expression (Glsl.Vec Float a) -> Glsl.Expression (Glsl.Vec Float a)
sinh a =
    Glsl.unsafeCall1 "sinh" [] a


cosh : Glsl.Expression (Glsl.Vec Float a) -> Glsl.Expression (Glsl.Vec Float a)
cosh a =
    Glsl.unsafeCall1 "cosh" [] a


tanh : Glsl.Expression (Glsl.Vec Float a) -> Glsl.Expression (Glsl.Vec Float a)
tanh a =
    Glsl.unsafeCall1 "tanh" [] a


asinh : Glsl.Expression (Glsl.Vec Float a) -> Glsl.Expression (Glsl.Vec Float a)
asinh a =
    Glsl.unsafeCall1 "asinh" [] a


acosh : Glsl.Expression (Glsl.Vec Float a) -> Glsl.Expression (Glsl.Vec Float a)
acosh a =
    Glsl.unsafeCall1 "acosh" [] a


atanh : Glsl.Expression (Glsl.Vec Float a) -> Glsl.Expression (Glsl.Vec Float a)
atanh a =
    Glsl.unsafeCall1 "atanh" [] a


pow :
    Glsl.Expression (Glsl.Vec Float a)
    -> Glsl.Expression (Glsl.Vec Float a)
    -> Glsl.Expression (Glsl.Vec Float a)
pow a b =
    Glsl.unsafeCall2 "pow" [] a b


exp : Glsl.Expression (Glsl.Vec Float a) -> Glsl.Expression (Glsl.Vec Float a)
exp a =
    Glsl.unsafeCall1 "exp" [] a


log : Glsl.Expression (Glsl.Vec Float a) -> Glsl.Expression (Glsl.Vec Float a)
log a =
    Glsl.unsafeCall1 "log" [] a


exp2 : Glsl.Expression (Glsl.Vec Float a) -> Glsl.Expression (Glsl.Vec Float a)
exp2 a =
    Glsl.unsafeCall1 "exp2" [] a


log2 : Glsl.Expression (Glsl.Vec Float a) -> Glsl.Expression (Glsl.Vec Float a)
log2 a =
    Glsl.unsafeCall1 "log2" [] a


sqrt_ : Glsl.Expression (Glsl.Vec t a) -> Glsl.Expression (Glsl.Vec t a)
sqrt_ a =
    Glsl.unsafeCall1 "sqrt" [] a


abs_ : Glsl.Expression (Glsl.Vec t a) -> Glsl.Expression (Glsl.Vec t a)
abs_ a =
    Glsl.unsafeCall1 "abs" [] a


floor_ : Glsl.Expression (Glsl.Vec t a) -> Glsl.Expression (Glsl.Vec t a)
floor_ a =
    Glsl.unsafeCall1 "floor" [] a


round_ : Glsl.Expression (Glsl.Vec t a) -> Glsl.Expression (Glsl.Vec t a)
round_ a =
    Glsl.unsafeCall1 "round" [] a


min_ :
    Glsl.Expression (Glsl.Vec t a)
    -> Glsl.Expression (Glsl.Vec t a)
    -> Glsl.Expression (Glsl.Vec t a)
min_ a b =
    Glsl.unsafeCall2 "min" [] a b


max_ :
    Glsl.Expression (Glsl.Vec t a)
    -> Glsl.Expression (Glsl.Vec t a)
    -> Glsl.Expression (Glsl.Vec t a)
max_ a b =
    Glsl.unsafeCall2 "max" [] a b


clamp_ :
    Glsl.Expression (Glsl.Vec t a)
    -> Glsl.Expression (Glsl.Vec t a)
    -> Glsl.Expression (Glsl.Vec t a)
    -> Glsl.Expression (Glsl.Vec t a)
clamp_ a b c =
    Glsl.unsafeCall3 "clamp" [] a b c


dFdx : Glsl.Expression (Glsl.Vec Float a) -> Glsl.Expression (Glsl.Vec Float a)
dFdx a =
    Glsl.unsafeCall1 "dFdx" [] a


dFdy : Glsl.Expression (Glsl.Vec Float a) -> Glsl.Expression (Glsl.Vec Float a)
dFdy a =
    Glsl.unsafeCall1 "dFdy" [] a


dFdxFine : Glsl.Expression (Glsl.Vec Float a) -> Glsl.Expression (Glsl.Vec Float a)
dFdxFine a =
    Glsl.unsafeCall1 "dFdxFine" [] a


dFdyFine : Glsl.Expression (Glsl.Vec Float a) -> Glsl.Expression (Glsl.Vec Float a)
dFdyFine a =
    Glsl.unsafeCall1 "dFdyFine" [] a


dFdxCoarse : Glsl.Expression (Glsl.Vec Float a) -> Glsl.Expression (Glsl.Vec Float a)
dFdxCoarse a =
    Glsl.unsafeCall1 "dFdxCoarse" [] a


dFdyCoarse : Glsl.Expression (Glsl.Vec Float a) -> Glsl.Expression (Glsl.Vec Float a)
dFdyCoarse a =
    Glsl.unsafeCall1 "dFdyCoarse" [] a


fwidth : Glsl.Expression (Glsl.Vec Float a) -> Glsl.Expression (Glsl.Vec Float a)
fwidth a =
    Glsl.unsafeCall1 "fwidth" [] a


fwidthFine : Glsl.Expression (Glsl.Vec Float a) -> Glsl.Expression (Glsl.Vec Float a)
fwidthFine a =
    Glsl.unsafeCall1 "fwidthFine" [] a


fwidthCoarse : Glsl.Expression (Glsl.Vec Float a) -> Glsl.Expression (Glsl.Vec Float a)
fwidthCoarse a =
    Glsl.unsafeCall1 "fwidthCoarse" [] a
