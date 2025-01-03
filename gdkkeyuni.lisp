#||
/* GDK - The GIMP Drawing Kit
 * Copyright (C) 1995-1997 Peter Mattis, Spencer Kimball and Josh MacDonald
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library. If not, see <http://www.gnu.org/licenses/>.
 */

/*
 * Modified by the GTK+ Team and others 1997-2000.  See the AUTHORS
 * file for a list of people on the GTK+ Team.  See the ChangeLog
 * files for a list of changes.  These files are distributed with
 * GTK+ at ftp://ftp.gtk.org/pub/gtk/.
 */
||#
(in-package "GDK-KEY")
#||
/* Thanks to Markus G. Kuhn <mkuhn@acm.org> for the ksysym<->Unicode
 * mapping functions, from the xterm sources.
 */

/* These tables could be compressed by contiguous ranges, but the benefit of doing so
 * is smallish. It would save about ~1000 bytes total.
 */

static const struct (
  unsigned short keysym;
  unsigned short ucs;
} gdk_keysym_to_unicode_tab[]
||#

(defvar *gdk-keysym-to-unicode-tab*
 '(
  ( #x01a1 #x0104 ) #|/*                     Aogonek Ą LATIN CAPITAL LETTER A WITH OGONEK */|#
  ( #x01a2 #x02d8 ) #|/*                       breve ˘ BREVE */|#
  ( #x01a3 #x0141 ) #|/*                     Lstroke Ł LATIN CAPITAL LETTER L WITH STROKE */|#
  ( #x01a5 #x013d ) #|/*                      Lcaron Ľ LATIN CAPITAL LETTER L WITH CARON */|#
  ( #x01a6 #x015a ) #|/*                      Sacute Ś LATIN CAPITAL LETTER S WITH ACUTE */|#
  ( #x01a9 #x0160 ) #|/*                      Scaron Š LATIN CAPITAL LETTER S WITH CARON */|#
  ( #x01aa #x015e ) #|/*                    Scedilla Ş LATIN CAPITAL LETTER S WITH CEDILLA */|#
  ( #x01ab #x0164 ) #|/*                      Tcaron Ť LATIN CAPITAL LETTER T WITH CARON */|#
  ( #x01ac #x0179 ) #|/*                      Zacute Ź LATIN CAPITAL LETTER Z WITH ACUTE */|#
  ( #x01ae #x017d ) #|/*                      Zcaron Ž LATIN CAPITAL LETTER Z WITH CARON */|#
  ( #x01af #x017b ) #|/*                   Zabovedot Ż LATIN CAPITAL LETTER Z WITH DOT ABOVE */|#
  ( #x01b1 #x0105 ) #|/*                     aogonek ą LATIN SMALL LETTER A WITH OGONEK */|#
  ( #x01b2 #x02db ) #|/*                      ogonek ˛ OGONEK */|#
  ( #x01b3 #x0142 ) #|/*                     lstroke ł LATIN SMALL LETTER L WITH STROKE */|#
  ( #x01b5 #x013e ) #|/*                      lcaron ľ LATIN SMALL LETTER L WITH CARON */|#
  ( #x01b6 #x015b ) #|/*                      sacute ś LATIN SMALL LETTER S WITH ACUTE */|#
  ( #x01b7 #x02c7 ) #|/*                       caron ˇ CARON */|#
  ( #x01b9 #x0161 ) #|/*                      scaron š LATIN SMALL LETTER S WITH CARON */|#
  ( #x01ba #x015f ) #|/*                    scedilla ş LATIN SMALL LETTER S WITH CEDILLA */|#
  ( #x01bb #x0165 ) #|/*                      tcaron ť LATIN SMALL LETTER T WITH CARON */|#
  ( #x01bc #x017a ) #|/*                      zacute ź LATIN SMALL LETTER Z WITH ACUTE */|#
  ( #x01bd #x02dd ) #|/*                 doubleacute ˝ DOUBLE ACUTE ACCENT */|#
  ( #x01be #x017e ) #|/*                      zcaron ž LATIN SMALL LETTER Z WITH CARON */|#
  ( #x01bf #x017c ) #|/*                   zabovedot ż LATIN SMALL LETTER Z WITH DOT ABOVE */|#
  ( #x01c0 #x0154 ) #|/*                      Racute Ŕ LATIN CAPITAL LETTER R WITH ACUTE */|#
  ( #x01c3 #x0102 ) #|/*                      Abreve Ă LATIN CAPITAL LETTER A WITH BREVE */|#
  ( #x01c5 #x0139 ) #|/*                      Lacute Ĺ LATIN CAPITAL LETTER L WITH ACUTE */|#
  ( #x01c6 #x0106 ) #|/*                      Cacute Ć LATIN CAPITAL LETTER C WITH ACUTE */|#
  ( #x01c8 #x010c ) #|/*                      Ccaron Č LATIN CAPITAL LETTER C WITH CARON */|#
  ( #x01ca #x0118 ) #|/*                     Eogonek Ę LATIN CAPITAL LETTER E WITH OGONEK */|#
  ( #x01cc #x011a ) #|/*                      Ecaron Ě LATIN CAPITAL LETTER E WITH CARON */|#
  ( #x01cf #x010e ) #|/*                      Dcaron Ď LATIN CAPITAL LETTER D WITH CARON */|#
  ( #x01d0 #x0110 ) #|/*                     Dstroke Đ LATIN CAPITAL LETTER D WITH STROKE */|#
  ( #x01d1 #x0143 ) #|/*                      Nacute Ń LATIN CAPITAL LETTER N WITH ACUTE */|#
  ( #x01d2 #x0147 ) #|/*                      Ncaron Ň LATIN CAPITAL LETTER N WITH CARON */|#
  ( #x01d5 #x0150 ) #|/*                Odoubleacute Ő LATIN CAPITAL LETTER O WITH DOUBLE ACUTE */|#
  ( #x01d8 #x0158 ) #|/*                      Rcaron Ř LATIN CAPITAL LETTER R WITH CARON */|#
  ( #x01d9 #x016e ) #|/*                       Uring Ů LATIN CAPITAL LETTER U WITH RING ABOVE */|#
  ( #x01db #x0170 ) #|/*                Udoubleacute Ű LATIN CAPITAL LETTER U WITH DOUBLE ACUTE */|#
  ( #x01de #x0162 ) #|/*                    Tcedilla Ţ LATIN CAPITAL LETTER T WITH CEDILLA */|#
  ( #x01e0 #x0155 ) #|/*                      racute ŕ LATIN SMALL LETTER R WITH ACUTE */|#
  ( #x01e3 #x0103 ) #|/*                      abreve ă LATIN SMALL LETTER A WITH BREVE */|#
  ( #x01e5 #x013a ) #|/*                      lacute ĺ LATIN SMALL LETTER L WITH ACUTE */|#
  ( #x01e6 #x0107 ) #|/*                      cacute ć LATIN SMALL LETTER C WITH ACUTE */|#
  ( #x01e8 #x010d ) #|/*                      ccaron č LATIN SMALL LETTER C WITH CARON */|#
  ( #x01ea #x0119 ) #|/*                     eogonek ę LATIN SMALL LETTER E WITH OGONEK */|#
  ( #x01ec #x011b ) #|/*                      ecaron ě LATIN SMALL LETTER E WITH CARON */|#
  ( #x01ef #x010f ) #|/*                      dcaron ď LATIN SMALL LETTER D WITH CARON */|#
  ( #x01f0 #x0111 ) #|/*                     dstroke đ LATIN SMALL LETTER D WITH STROKE */|#
  ( #x01f1 #x0144 ) #|/*                      nacute ń LATIN SMALL LETTER N WITH ACUTE */|#
  ( #x01f2 #x0148 ) #|/*                      ncaron ň LATIN SMALL LETTER N WITH CARON */|#
  ( #x01f5 #x0151 ) #|/*                odoubleacute ő LATIN SMALL LETTER O WITH DOUBLE ACUTE */|#
  ( #x01f8 #x0159 ) #|/*                      rcaron ř LATIN SMALL LETTER R WITH CARON */|#
  ( #x01f9 #x016f ) #|/*                       uring ů LATIN SMALL LETTER U WITH RING ABOVE */|#
  ( #x01fb #x0171 ) #|/*                udoubleacute ű LATIN SMALL LETTER U WITH DOUBLE ACUTE */|#
  ( #x01fe #x0163 ) #|/*                    tcedilla ţ LATIN SMALL LETTER T WITH CEDILLA */|#
  ( #x01ff #x02d9 ) #|/*                    abovedot ˙ DOT ABOVE */|#
  ( #x02a1 #x0126 ) #|/*                     Hstroke Ħ LATIN CAPITAL LETTER H WITH STROKE */|#
  ( #x02a6 #x0124 ) #|/*                 Hcircumflex Ĥ LATIN CAPITAL LETTER H WITH CIRCUMFLEX */|#
  ( #x02a9 #x0130 ) #|/*                   Iabovedot İ LATIN CAPITAL LETTER I WITH DOT ABOVE */|#
  ( #x02ab #x011e ) #|/*                      Gbreve Ğ LATIN CAPITAL LETTER G WITH BREVE */|#
  ( #x02ac #x0134 ) #|/*                 Jcircumflex Ĵ LATIN CAPITAL LETTER J WITH CIRCUMFLEX */|#
  ( #x02b1 #x0127 ) #|/*                     hstroke ħ LATIN SMALL LETTER H WITH STROKE */|#
  ( #x02b6 #x0125 ) #|/*                 hcircumflex ĥ LATIN SMALL LETTER H WITH CIRCUMFLEX */|#
  ( #x02b9 #x0131 ) #|/*                    idotless ı LATIN SMALL LETTER DOTLESS I */|#
  ( #x02bb #x011f ) #|/*                      gbreve ğ LATIN SMALL LETTER G WITH BREVE */|#
  ( #x02bc #x0135 ) #|/*                 jcircumflex ĵ LATIN SMALL LETTER J WITH CIRCUMFLEX */|#
  ( #x02c5 #x010a ) #|/*                   Cabovedot Ċ LATIN CAPITAL LETTER C WITH DOT ABOVE */|#
  ( #x02c6 #x0108 ) #|/*                 Ccircumflex Ĉ LATIN CAPITAL LETTER C WITH CIRCUMFLEX */|#
  ( #x02d5 #x0120 ) #|/*                   Gabovedot Ġ LATIN CAPITAL LETTER G WITH DOT ABOVE */|#
  ( #x02d8 #x011c ) #|/*                 Gcircumflex Ĝ LATIN CAPITAL LETTER G WITH CIRCUMFLEX */|#
  ( #x02dd #x016c ) #|/*                      Ubreve Ŭ LATIN CAPITAL LETTER U WITH BREVE */|#
  ( #x02de #x015c ) #|/*                 Scircumflex Ŝ LATIN CAPITAL LETTER S WITH CIRCUMFLEX */|#
  ( #x02e5 #x010b ) #|/*                   cabovedot ċ LATIN SMALL LETTER C WITH DOT ABOVE */|#
  ( #x02e6 #x0109 ) #|/*                 ccircumflex ĉ LATIN SMALL LETTER C WITH CIRCUMFLEX */|#
  ( #x02f5 #x0121 ) #|/*                   gabovedot ġ LATIN SMALL LETTER G WITH DOT ABOVE */|#
  ( #x02f8 #x011d ) #|/*                 gcircumflex ĝ LATIN SMALL LETTER G WITH CIRCUMFLEX */|#
  ( #x02fd #x016d ) #|/*                      ubreve ŭ LATIN SMALL LETTER U WITH BREVE */|#
  ( #x02fe #x015d ) #|/*                 scircumflex ŝ LATIN SMALL LETTER S WITH CIRCUMFLEX */|#
  ( #x03a2 #x0138 ) #|/*                         kra ĸ LATIN SMALL LETTER KRA */|#
  ( #x03a3 #x0156 ) #|/*                    Rcedilla Ŗ LATIN CAPITAL LETTER R WITH CEDILLA */|#
  ( #x03a5 #x0128 ) #|/*                      Itilde Ĩ LATIN CAPITAL LETTER I WITH TILDE */|#
  ( #x03a6 #x013b ) #|/*                    Lcedilla Ļ LATIN CAPITAL LETTER L WITH CEDILLA */|#
  ( #x03aa #x0112 ) #|/*                     Emacron Ē LATIN CAPITAL LETTER E WITH MACRON */|#
  ( #x03ab #x0122 ) #|/*                    Gcedilla Ģ LATIN CAPITAL LETTER G WITH CEDILLA */|#
  ( #x03ac #x0166 ) #|/*                      Tslash Ŧ LATIN CAPITAL LETTER T WITH STROKE */|#
  ( #x03b3 #x0157 ) #|/*                    rcedilla ŗ LATIN SMALL LETTER R WITH CEDILLA */|#
  ( #x03b5 #x0129 ) #|/*                      itilde ĩ LATIN SMALL LETTER I WITH TILDE */|#
  ( #x03b6 #x013c ) #|/*                    lcedilla ļ LATIN SMALL LETTER L WITH CEDILLA */|#
  ( #x03ba #x0113 ) #|/*                     emacron ē LATIN SMALL LETTER E WITH MACRON */|#
  ( #x03bb #x0123 ) #|/*                    gcedilla ģ LATIN SMALL LETTER G WITH CEDILLA */|#
  ( #x03bc #x0167 ) #|/*                      tslash ŧ LATIN SMALL LETTER T WITH STROKE */|#
  ( #x03bd #x014a ) #|/*                         ENG Ŋ LATIN CAPITAL LETTER ENG */|#
  ( #x03bf #x014b ) #|/*                         eng ŋ LATIN SMALL LETTER ENG */|#
  ( #x03c0 #x0100 ) #|/*                     Amacron Ā LATIN CAPITAL LETTER A WITH MACRON */|#
  ( #x03c7 #x012e ) #|/*                     Iogonek Į LATIN CAPITAL LETTER I WITH OGONEK */|#
  ( #x03cc #x0116 ) #|/*                   Eabovedot Ė LATIN CAPITAL LETTER E WITH DOT ABOVE */|#
  ( #x03cf #x012a ) #|/*                     Imacron Ī LATIN CAPITAL LETTER I WITH MACRON */|#
  ( #x03d1 #x0145 ) #|/*                    Ncedilla Ņ LATIN CAPITAL LETTER N WITH CEDILLA */|#
  ( #x03d2 #x014c ) #|/*                     Omacron Ō LATIN CAPITAL LETTER O WITH MACRON */|#
  ( #x03d3 #x0136 ) #|/*                    Kcedilla Ķ LATIN CAPITAL LETTER K WITH CEDILLA */|#
  ( #x03d9 #x0172 ) #|/*                     Uogonek Ų LATIN CAPITAL LETTER U WITH OGONEK */|#
  ( #x03dd #x0168 ) #|/*                      Utilde Ũ LATIN CAPITAL LETTER U WITH TILDE */|#
  ( #x03de #x016a ) #|/*                     Umacron Ū LATIN CAPITAL LETTER U WITH MACRON */|#
  ( #x03e0 #x0101 ) #|/*                     amacron ā LATIN SMALL LETTER A WITH MACRON */|#
  ( #x03e7 #x012f ) #|/*                     iogonek į LATIN SMALL LETTER I WITH OGONEK */|#
  ( #x03ec #x0117 ) #|/*                   eabovedot ė LATIN SMALL LETTER E WITH DOT ABOVE */|#
  ( #x03ef #x012b ) #|/*                     imacron ī LATIN SMALL LETTER I WITH MACRON */|#
  ( #x03f1 #x0146 ) #|/*                    ncedilla ņ LATIN SMALL LETTER N WITH CEDILLA */|#
  ( #x03f2 #x014d ) #|/*                     omacron ō LATIN SMALL LETTER O WITH MACRON */|#
  ( #x03f3 #x0137 ) #|/*                    kcedilla ķ LATIN SMALL LETTER K WITH CEDILLA */|#
  ( #x03f9 #x0173 ) #|/*                     uogonek ų LATIN SMALL LETTER U WITH OGONEK */|#
  ( #x03fd #x0169 ) #|/*                      utilde ũ LATIN SMALL LETTER U WITH TILDE */|#
  ( #x03fe #x016b ) #|/*                     umacron ū LATIN SMALL LETTER U WITH MACRON */|#
  ( #x047e #x203e ) #|/*                    overline ‾ OVERLINE */|#
  ( #x04a1 #x3002 ) #|/*               kana_fullstop 。 IDEOGRAPHIC FULL STOP */|#
  ( #x04a2 #x300c ) #|/*         kana_openingbracket 「 LEFT CORNER BRACKET */|#
  ( #x04a3 #x300d ) #|/*         kana_closingbracket 」 RIGHT CORNER BRACKET */|#
  ( #x04a4 #x3001 ) #|/*                  kana_comma 、 IDEOGRAPHIC COMMA */|#
  ( #x04a5 #x30fb ) #|/*            kana_conjunctive ・ KATAKANA MIDDLE DOT */|#
  ( #x04a6 #x30f2 ) #|/*                     kana_WO ヲ KATAKANA LETTER WO */|#
  ( #x04a7 #x30a1 ) #|/*                      kana_a ァ KATAKANA LETTER SMALL A */|#
  ( #x04a8 #x30a3 ) #|/*                      kana_i ィ KATAKANA LETTER SMALL I */|#
  ( #x04a9 #x30a5 ) #|/*                      kana_u ゥ KATAKANA LETTER SMALL U */|#
  ( #x04aa #x30a7 ) #|/*                      kana_e ェ KATAKANA LETTER SMALL E */|#
  ( #x04ab #x30a9 ) #|/*                      kana_o ォ KATAKANA LETTER SMALL O */|#
  ( #x04ac #x30e3 ) #|/*                     kana_ya ャ KATAKANA LETTER SMALL YA */|#
  ( #x04ad #x30e5 ) #|/*                     kana_yu ュ KATAKANA LETTER SMALL YU */|#
  ( #x04ae #x30e7 ) #|/*                     kana_yo ョ KATAKANA LETTER SMALL YO */|#
  ( #x04af #x30c3 ) #|/*                    kana_tsu ッ KATAKANA LETTER SMALL TU */|#
  ( #x04b0 #x30fc ) #|/*              prolongedsound ー KATAKANA-HIRAGANA PROLONGED SOUND MARK */|#
  ( #x04b1 #x30a2 ) #|/*                      kana_A ア KATAKANA LETTER A */|#
  ( #x04b2 #x30a4 ) #|/*                      kana_I イ KATAKANA LETTER I */|#
  ( #x04b3 #x30a6 ) #|/*                      kana_U ウ KATAKANA LETTER U */|#
  ( #x04b4 #x30a8 ) #|/*                      kana_E エ KATAKANA LETTER E */|#
  ( #x04b5 #x30aa ) #|/*                      kana_O オ KATAKANA LETTER O */|#
  ( #x04b6 #x30ab ) #|/*                     kana_KA カ KATAKANA LETTER KA */|#
  ( #x04b7 #x30ad ) #|/*                     kana_KI キ KATAKANA LETTER KI */|#
  ( #x04b8 #x30af ) #|/*                     kana_KU ク KATAKANA LETTER KU */|#
  ( #x04b9 #x30b1 ) #|/*                     kana_KE ケ KATAKANA LETTER KE */|#
  ( #x04ba #x30b3 ) #|/*                     kana_KO コ KATAKANA LETTER KO */|#
  ( #x04bb #x30b5 ) #|/*                     kana_SA サ KATAKANA LETTER SA */|#
  ( #x04bc #x30b7 ) #|/*                    kana_SHI シ KATAKANA LETTER SI */|#
  ( #x04bd #x30b9 ) #|/*                     kana_SU ス KATAKANA LETTER SU */|#
  ( #x04be #x30bb ) #|/*                     kana_SE セ KATAKANA LETTER SE */|#
  ( #x04bf #x30bd ) #|/*                     kana_SO ソ KATAKANA LETTER SO */|#
  ( #x04c0 #x30bf ) #|/*                     kana_TA タ KATAKANA LETTER TA */|#
  ( #x04c1 #x30c1 ) #|/*                    kana_CHI チ KATAKANA LETTER TI */|#
  ( #x04c2 #x30c4 ) #|/*                    kana_TSU ツ KATAKANA LETTER TU */|#
  ( #x04c3 #x30c6 ) #|/*                     kana_TE テ KATAKANA LETTER TE */|#
  ( #x04c4 #x30c8 ) #|/*                     kana_TO ト KATAKANA LETTER TO */|#
  ( #x04c5 #x30ca ) #|/*                     kana_NA ナ KATAKANA LETTER NA */|#
  ( #x04c6 #x30cb ) #|/*                     kana_NI ニ KATAKANA LETTER NI */|#
  ( #x04c7 #x30cc ) #|/*                     kana_NU ヌ KATAKANA LETTER NU */|#
  ( #x04c8 #x30cd ) #|/*                     kana_NE ネ KATAKANA LETTER NE */|#
  ( #x04c9 #x30ce ) #|/*                     kana_NO ノ KATAKANA LETTER NO */|#
  ( #x04ca #x30cf ) #|/*                     kana_HA ハ KATAKANA LETTER HA */|#
  ( #x04cb #x30d2 ) #|/*                     kana_HI ヒ KATAKANA LETTER HI */|#
  ( #x04cc #x30d5 ) #|/*                     kana_FU フ KATAKANA LETTER HU */|#
  ( #x04cd #x30d8 ) #|/*                     kana_HE ヘ KATAKANA LETTER HE */|#
  ( #x04ce #x30db ) #|/*                     kana_HO ホ KATAKANA LETTER HO */|#
  ( #x04cf #x30de ) #|/*                     kana_MA マ KATAKANA LETTER MA */|#
  ( #x04d0 #x30df ) #|/*                     kana_MI ミ KATAKANA LETTER MI */|#
  ( #x04d1 #x30e0 ) #|/*                     kana_MU ム KATAKANA LETTER MU */|#
  ( #x04d2 #x30e1 ) #|/*                     kana_ME メ KATAKANA LETTER ME */|#
  ( #x04d3 #x30e2 ) #|/*                     kana_MO モ KATAKANA LETTER MO */|#
  ( #x04d4 #x30e4 ) #|/*                     kana_YA ヤ KATAKANA LETTER YA */|#
  ( #x04d5 #x30e6 ) #|/*                     kana_YU ユ KATAKANA LETTER YU */|#
  ( #x04d6 #x30e8 ) #|/*                     kana_YO ヨ KATAKANA LETTER YO */|#
  ( #x04d7 #x30e9 ) #|/*                     kana_RA ラ KATAKANA LETTER RA */|#
  ( #x04d8 #x30ea ) #|/*                     kana_RI リ KATAKANA LETTER RI */|#
  ( #x04d9 #x30eb ) #|/*                     kana_RU ル KATAKANA LETTER RU */|#
  ( #x04da #x30ec ) #|/*                     kana_RE レ KATAKANA LETTER RE */|#
  ( #x04db #x30ed ) #|/*                     kana_RO ロ KATAKANA LETTER RO */|#
  ( #x04dc #x30ef ) #|/*                     kana_WA ワ KATAKANA LETTER WA */|#
  ( #x04dd #x30f3 ) #|/*                      kana_N ン KATAKANA LETTER N */|#
  ( #x04de #x309b ) #|/*                 voicedsound ゛ KATAKANA-HIRAGANA VOICED SOUND MARK */|#
  ( #x04df #x309c ) #|/*             semivoicedsound ゜ KATAKANA-HIRAGANA SEMI-VOICED SOUND MARK */|#
  ( #x05ac #x060c ) #|/*                Arabic_comma ، ARABIC COMMA */|#
  ( #x05bb #x061b ) #|/*            Arabic_semicolon ؛ ARABIC SEMICOLON */|#
  ( #x05bf #x061f ) #|/*        Arabic_question_mark ؟ ARABIC QUESTION MARK */|#
  ( #x05c1 #x0621 ) #|/*                Arabic_hamza ء ARABIC LETTER HAMZA */|#
  ( #x05c2 #x0622 ) #|/*          Arabic_maddaonalef آ ARABIC LETTER ALEF WITH MADDA ABOVE */|#
  ( #x05c3 #x0623 ) #|/*          Arabic_hamzaonalef أ ARABIC LETTER ALEF WITH HAMZA ABOVE */|#
  ( #x05c4 #x0624 ) #|/*           Arabic_hamzaonwaw ؤ ARABIC LETTER WAW WITH HAMZA ABOVE */|#
  ( #x05c5 #x0625 ) #|/*       Arabic_hamzaunderalef إ ARABIC LETTER ALEF WITH HAMZA BELOW */|#
  ( #x05c6 #x0626 ) #|/*           Arabic_hamzaonyeh ئ ARABIC LETTER YEH WITH HAMZA ABOVE */|#
  ( #x05c7 #x0627 ) #|/*                 Arabic_alef ا ARABIC LETTER ALEF */|#
  ( #x05c8 #x0628 ) #|/*                  Arabic_beh ب ARABIC LETTER BEH */|#
  ( #x05c9 #x0629 ) #|/*           Arabic_tehmarbuta ة ARABIC LETTER TEH MARBUTA */|#
  ( #x05ca #x062a ) #|/*                  Arabic_teh ت ARABIC LETTER TEH */|#
  ( #x05cb #x062b ) #|/*                 Arabic_theh ث ARABIC LETTER THEH */|#
  ( #x05cc #x062c ) #|/*                 Arabic_jeem ج ARABIC LETTER JEEM */|#
  ( #x05cd #x062d ) #|/*                  Arabic_hah ح ARABIC LETTER HAH */|#
  ( #x05ce #x062e ) #|/*                 Arabic_khah خ ARABIC LETTER KHAH */|#
  ( #x05cf #x062f ) #|/*                  Arabic_dal د ARABIC LETTER DAL */|#
  ( #x05d0 #x0630 ) #|/*                 Arabic_thal ذ ARABIC LETTER THAL */|#
  ( #x05d1 #x0631 ) #|/*                   Arabic_ra ر ARABIC LETTER REH */|#
  ( #x05d2 #x0632 ) #|/*                 Arabic_zain ز ARABIC LETTER ZAIN */|#
  ( #x05d3 #x0633 ) #|/*                 Arabic_seen س ARABIC LETTER SEEN */|#
  ( #x05d4 #x0634 ) #|/*                Arabic_sheen ش ARABIC LETTER SHEEN */|#
  ( #x05d5 #x0635 ) #|/*                  Arabic_sad ص ARABIC LETTER SAD */|#
  ( #x05d6 #x0636 ) #|/*                  Arabic_dad ض ARABIC LETTER DAD */|#
  ( #x05d7 #x0637 ) #|/*                  Arabic_tah ط ARABIC LETTER TAH */|#
  ( #x05d8 #x0638 ) #|/*                  Arabic_zah ظ ARABIC LETTER ZAH */|#
  ( #x05d9 #x0639 ) #|/*                  Arabic_ain ع ARABIC LETTER AIN */|#
  ( #x05da #x063a ) #|/*                Arabic_ghain غ ARABIC LETTER GHAIN */|#
  ( #x05e0 #x0640 ) #|/*              Arabic_tatweel ـ ARABIC TATWEEL */|#
  ( #x05e1 #x0641 ) #|/*                  Arabic_feh ف ARABIC LETTER FEH */|#
  ( #x05e2 #x0642 ) #|/*                  Arabic_qaf ق ARABIC LETTER QAF */|#
  ( #x05e3 #x0643 ) #|/*                  Arabic_kaf ك ARABIC LETTER KAF */|#
  ( #x05e4 #x0644 ) #|/*                  Arabic_lam ل ARABIC LETTER LAM */|#
  ( #x05e5 #x0645 ) #|/*                 Arabic_meem م ARABIC LETTER MEEM */|#
  ( #x05e6 #x0646 ) #|/*                 Arabic_noon ن ARABIC LETTER NOON */|#
  ( #x05e7 #x0647 ) #|/*                   Arabic_ha ه ARABIC LETTER HEH */|#
  ( #x05e8 #x0648 ) #|/*                  Arabic_waw و ARABIC LETTER WAW */|#
  ( #x05e9 #x0649 ) #|/*          Arabic_alefmaksura ى ARABIC LETTER ALEF MAKSURA */|#
  ( #x05ea #x064a ) #|/*                  Arabic_yeh ي ARABIC LETTER YEH */|#
  ( #x05eb #x064b ) #|/*             Arabic_fathatan ً ARABIC FATHATAN */|#
  ( #x05ec #x064c ) #|/*             Arabic_dammatan ٌ ARABIC DAMMATAN */|#
  ( #x05ed #x064d ) #|/*             Arabic_kasratan ٍ ARABIC KASRATAN */|#
  ( #x05ee #x064e ) #|/*                Arabic_fatha َ ARABIC FATHA */|#
  ( #x05ef #x064f ) #|/*                Arabic_damma ُ ARABIC DAMMA */|#
  ( #x05f0 #x0650 ) #|/*                Arabic_kasra ِ ARABIC KASRA */|#
  ( #x05f1 #x0651 ) #|/*               Arabic_shadda ّ ARABIC SHADDA */|#
  ( #x05f2 #x0652 ) #|/*                Arabic_sukun ْ ARABIC SUKUN */|#
  ( #x06a1 #x0452 ) #|/*                 Serbian_dje ђ CYRILLIC SMALL LETTER DJE */|#
  ( #x06a2 #x0453 ) #|/*               Macedonia_gje ѓ CYRILLIC SMALL LETTER GJE */|#
  ( #x06a3 #x0451 ) #|/*                 Cyrillic_io ё CYRILLIC SMALL LETTER IO */|#
  ( #x06a4 #x0454 ) #|/*                Ukrainian_ie є CYRILLIC SMALL LETTER UKRAINIAN IE */|#
  ( #x06a5 #x0455 ) #|/*               Macedonia_dse ѕ CYRILLIC SMALL LETTER DZE */|#
  ( #x06a6 #x0456 ) #|/*                 Ukrainian_i і CYRILLIC SMALL LETTER BYELORUSSIAN-UKRAINIAN I */|#
  ( #x06a7 #x0457 ) #|/*                Ukrainian_yi ї CYRILLIC SMALL LETTER YI */|#
  ( #x06a8 #x0458 ) #|/*                 Cyrillic_je ј CYRILLIC SMALL LETTER JE */|#
  ( #x06a9 #x0459 ) #|/*                Cyrillic_lje љ CYRILLIC SMALL LETTER LJE */|#
  ( #x06aa #x045a ) #|/*                Cyrillic_nje њ CYRILLIC SMALL LETTER NJE */|#
  ( #x06ab #x045b ) #|/*                Serbian_tshe ћ CYRILLIC SMALL LETTER TSHE */|#
  ( #x06ac #x045c ) #|/*               Macedonia_kje ќ CYRILLIC SMALL LETTER KJE */|#
  ( #x06ad #x0491 ) #|/*   Ukrainian_ghe_with_upturn ґ CYRILLIC SMALL LETTER GHE WITH UPTURN */|#
  ( #x06ae #x045e ) #|/*         Byelorussian_shortu ў CYRILLIC SMALL LETTER SHORT U */|#
  ( #x06af #x045f ) #|/*               Cyrillic_dzhe џ CYRILLIC SMALL LETTER DZHE */|#
  ( #x06b0 #x2116 ) #|/*                  numerosign № NUMERO SIGN */|#
  ( #x06b1 #x0402 ) #|/*                 Serbian_DJE Ђ CYRILLIC CAPITAL LETTER DJE */|#
  ( #x06b2 #x0403 ) #|/*               Macedonia_GJE Ѓ CYRILLIC CAPITAL LETTER GJE */|#
  ( #x06b3 #x0401 ) #|/*                 Cyrillic_IO Ё CYRILLIC CAPITAL LETTER IO */|#
  ( #x06b4 #x0404 ) #|/*                Ukrainian_IE Є CYRILLIC CAPITAL LETTER UKRAINIAN IE */|#
  ( #x06b5 #x0405 ) #|/*               Macedonia_DSE Ѕ CYRILLIC CAPITAL LETTER DZE */|#
  ( #x06b6 #x0406 ) #|/*                 Ukrainian_I І CYRILLIC CAPITAL LETTER BYELORUSSIAN-UKRAINIAN I */|#
  ( #x06b7 #x0407 ) #|/*                Ukrainian_YI Ї CYRILLIC CAPITAL LETTER YI */|#
  ( #x06b8 #x0408 ) #|/*                 Cyrillic_JE Ј CYRILLIC CAPITAL LETTER JE */|#
  ( #x06b9 #x0409 ) #|/*                Cyrillic_LJE Љ CYRILLIC CAPITAL LETTER LJE */|#
  ( #x06ba #x040a ) #|/*                Cyrillic_NJE Њ CYRILLIC CAPITAL LETTER NJE */|#
  ( #x06bb #x040b ) #|/*                Serbian_TSHE Ћ CYRILLIC CAPITAL LETTER TSHE */|#
  ( #x06bc #x040c ) #|/*               Macedonia_KJE Ќ CYRILLIC CAPITAL LETTER KJE */|#
  ( #x06bd #x0490 ) #|/*   Ukrainian_GHE_WITH_UPTURN Ґ CYRILLIC CAPITAL LETTER GHE WITH UPTURN */|#
  ( #x06be #x040e ) #|/*         Byelorussian_SHORTU Ў CYRILLIC CAPITAL LETTER SHORT U */|#
  ( #x06bf #x040f ) #|/*               Cyrillic_DZHE Џ CYRILLIC CAPITAL LETTER DZHE */|#
  ( #x06c0 #x044e ) #|/*                 Cyrillic_yu ю CYRILLIC SMALL LETTER YU */|#
  ( #x06c1 #x0430 ) #|/*                  Cyrillic_a а CYRILLIC SMALL LETTER A */|#
  ( #x06c2 #x0431 ) #|/*                 Cyrillic_be б CYRILLIC SMALL LETTER BE */|#
  ( #x06c3 #x0446 ) #|/*                Cyrillic_tse ц CYRILLIC SMALL LETTER TSE */|#
  ( #x06c4 #x0434 ) #|/*                 Cyrillic_de д CYRILLIC SMALL LETTER DE */|#
  ( #x06c5 #x0435 ) #|/*                 Cyrillic_ie е CYRILLIC SMALL LETTER IE */|#
  ( #x06c6 #x0444 ) #|/*                 Cyrillic_ef ф CYRILLIC SMALL LETTER EF */|#
  ( #x06c7 #x0433 ) #|/*                Cyrillic_ghe г CYRILLIC SMALL LETTER GHE */|#
  ( #x06c8 #x0445 ) #|/*                 Cyrillic_ha х CYRILLIC SMALL LETTER HA */|#
  ( #x06c9 #x0438 ) #|/*                  Cyrillic_i и CYRILLIC SMALL LETTER I */|#
  ( #x06ca #x0439 ) #|/*             Cyrillic_shorti й CYRILLIC SMALL LETTER SHORT I */|#
  ( #x06cb #x043a ) #|/*                 Cyrillic_ka к CYRILLIC SMALL LETTER KA */|#
  ( #x06cc #x043b ) #|/*                 Cyrillic_el л CYRILLIC SMALL LETTER EL */|#
  ( #x06cd #x043c ) #|/*                 Cyrillic_em м CYRILLIC SMALL LETTER EM */|#
  ( #x06ce #x043d ) #|/*                 Cyrillic_en н CYRILLIC SMALL LETTER EN */|#
  ( #x06cf #x043e ) #|/*                  Cyrillic_o о CYRILLIC SMALL LETTER O */|#
  ( #x06d0 #x043f ) #|/*                 Cyrillic_pe п CYRILLIC SMALL LETTER PE */|#
  ( #x06d1 #x044f ) #|/*                 Cyrillic_ya я CYRILLIC SMALL LETTER YA */|#
  ( #x06d2 #x0440 ) #|/*                 Cyrillic_er р CYRILLIC SMALL LETTER ER */|#
  ( #x06d3 #x0441 ) #|/*                 Cyrillic_es с CYRILLIC SMALL LETTER ES */|#
  ( #x06d4 #x0442 ) #|/*                 Cyrillic_te т CYRILLIC SMALL LETTER TE */|#
  ( #x06d5 #x0443 ) #|/*                  Cyrillic_u у CYRILLIC SMALL LETTER U */|#
  ( #x06d6 #x0436 ) #|/*                Cyrillic_zhe ж CYRILLIC SMALL LETTER ZHE */|#
  ( #x06d7 #x0432 ) #|/*                 Cyrillic_ve в CYRILLIC SMALL LETTER VE */|#
  ( #x06d8 #x044c ) #|/*           Cyrillic_softsign ь CYRILLIC SMALL LETTER SOFT SIGN */|#
  ( #x06d9 #x044b ) #|/*               Cyrillic_yeru ы CYRILLIC SMALL LETTER YERU */|#
  ( #x06da #x0437 ) #|/*                 Cyrillic_ze з CYRILLIC SMALL LETTER ZE */|#
  ( #x06db #x0448 ) #|/*                Cyrillic_sha ш CYRILLIC SMALL LETTER SHA */|#
  ( #x06dc #x044d ) #|/*                  Cyrillic_e э CYRILLIC SMALL LETTER E */|#
  ( #x06dd #x0449 ) #|/*              Cyrillic_shcha щ CYRILLIC SMALL LETTER SHCHA */|#
  ( #x06de #x0447 ) #|/*                Cyrillic_che ч CYRILLIC SMALL LETTER CHE */|#
  ( #x06df #x044a ) #|/*           Cyrillic_hardsign ъ CYRILLIC SMALL LETTER HARD SIGN */|#
  ( #x06e0 #x042e ) #|/*                 Cyrillic_YU Ю CYRILLIC CAPITAL LETTER YU */|#
  ( #x06e1 #x0410 ) #|/*                  Cyrillic_A А CYRILLIC CAPITAL LETTER A */|#
  ( #x06e2 #x0411 ) #|/*                 Cyrillic_BE Б CYRILLIC CAPITAL LETTER BE */|#
  ( #x06e3 #x0426 ) #|/*                Cyrillic_TSE Ц CYRILLIC CAPITAL LETTER TSE */|#
  ( #x06e4 #x0414 ) #|/*                 Cyrillic_DE Д CYRILLIC CAPITAL LETTER DE */|#
  ( #x06e5 #x0415 ) #|/*                 Cyrillic_IE Е CYRILLIC CAPITAL LETTER IE */|#
  ( #x06e6 #x0424 ) #|/*                 Cyrillic_EF Ф CYRILLIC CAPITAL LETTER EF */|#
  ( #x06e7 #x0413 ) #|/*                Cyrillic_GHE Г CYRILLIC CAPITAL LETTER GHE */|#
  ( #x06e8 #x0425 ) #|/*                 Cyrillic_HA Х CYRILLIC CAPITAL LETTER HA */|#
  ( #x06e9 #x0418 ) #|/*                  Cyrillic_I И CYRILLIC CAPITAL LETTER I */|#
  ( #x06ea #x0419 ) #|/*             Cyrillic_SHORTI Й CYRILLIC CAPITAL LETTER SHORT I */|#
  ( #x06eb #x041a ) #|/*                 Cyrillic_KA К CYRILLIC CAPITAL LETTER KA */|#
  ( #x06ec #x041b ) #|/*                 Cyrillic_EL Л CYRILLIC CAPITAL LETTER EL */|#
  ( #x06ed #x041c ) #|/*                 Cyrillic_EM М CYRILLIC CAPITAL LETTER EM */|#
  ( #x06ee #x041d ) #|/*                 Cyrillic_EN Н CYRILLIC CAPITAL LETTER EN */|#
  ( #x06ef #x041e ) #|/*                  Cyrillic_O О CYRILLIC CAPITAL LETTER O */|#
  ( #x06f0 #x041f ) #|/*                 Cyrillic_PE П CYRILLIC CAPITAL LETTER PE */|#
  ( #x06f1 #x042f ) #|/*                 Cyrillic_YA Я CYRILLIC CAPITAL LETTER YA */|#
  ( #x06f2 #x0420 ) #|/*                 Cyrillic_ER Р CYRILLIC CAPITAL LETTER ER */|#
  ( #x06f3 #x0421 ) #|/*                 Cyrillic_ES С CYRILLIC CAPITAL LETTER ES */|#
  ( #x06f4 #x0422 ) #|/*                 Cyrillic_TE Т CYRILLIC CAPITAL LETTER TE */|#
  ( #x06f5 #x0423 ) #|/*                  Cyrillic_U У CYRILLIC CAPITAL LETTER U */|#
  ( #x06f6 #x0416 ) #|/*                Cyrillic_ZHE Ж CYRILLIC CAPITAL LETTER ZHE */|#
  ( #x06f7 #x0412 ) #|/*                 Cyrillic_VE В CYRILLIC CAPITAL LETTER VE */|#
  ( #x06f8 #x042c ) #|/*           Cyrillic_SOFTSIGN Ь CYRILLIC CAPITAL LETTER SOFT SIGN */|#
  ( #x06f9 #x042b ) #|/*               Cyrillic_YERU Ы CYRILLIC CAPITAL LETTER YERU */|#
  ( #x06fa #x0417 ) #|/*                 Cyrillic_ZE З CYRILLIC CAPITAL LETTER ZE */|#
  ( #x06fb #x0428 ) #|/*                Cyrillic_SHA Ш CYRILLIC CAPITAL LETTER SHA */|#
  ( #x06fc #x042d ) #|/*                  Cyrillic_E Э CYRILLIC CAPITAL LETTER E */|#
  ( #x06fd #x0429 ) #|/*              Cyrillic_SHCHA Щ CYRILLIC CAPITAL LETTER SHCHA */|#
  ( #x06fe #x0427 ) #|/*                Cyrillic_CHE Ч CYRILLIC CAPITAL LETTER CHE */|#
  ( #x06ff #x042a ) #|/*           Cyrillic_HARDSIGN Ъ CYRILLIC CAPITAL LETTER HARD SIGN */|#
  ( #x07a1 #x0386 ) #|/*           Greek_ALPHAaccent Ά GREEK CAPITAL LETTER ALPHA WITH TONOS */|#
  ( #x07a2 #x0388 ) #|/*         Greek_EPSILONaccent Έ GREEK CAPITAL LETTER EPSILON WITH TONOS */|#
  ( #x07a3 #x0389 ) #|/*             Greek_ETAaccent Ή GREEK CAPITAL LETTER ETA WITH TONOS */|#
  ( #x07a4 #x038a ) #|/*            Greek_IOTAaccent Ί GREEK CAPITAL LETTER IOTA WITH TONOS */|#
  ( #x07a5 #x03aa ) #|/*         Greek_IOTAdiaeresis Ϊ GREEK CAPITAL LETTER IOTA WITH DIALYTIKA */|#
  ( #x07a7 #x038c ) #|/*         Greek_OMICRONaccent Ό GREEK CAPITAL LETTER OMICRON WITH TONOS */|#
  ( #x07a8 #x038e ) #|/*         Greek_UPSILONaccent Ύ GREEK CAPITAL LETTER UPSILON WITH TONOS */|#
  ( #x07a9 #x03ab ) #|/*       Greek_UPSILONdieresis Ϋ GREEK CAPITAL LETTER UPSILON WITH DIALYTIKA */|#
  ( #x07ab #x038f ) #|/*           Greek_OMEGAaccent Ώ GREEK CAPITAL LETTER OMEGA WITH TONOS */|#
  ( #x07ae #x0385 ) #|/*        Greek_accentdieresis ΅ GREEK DIALYTIKA TONOS */|#
  ( #x07af #x2015 ) #|/*              Greek_horizbar ― HORIZONTAL BAR */|#
  ( #x07b1 #x03ac ) #|/*           Greek_alphaaccent ά GREEK SMALL LETTER ALPHA WITH TONOS */|#
  ( #x07b2 #x03ad ) #|/*         Greek_epsilonaccent έ GREEK SMALL LETTER EPSILON WITH TONOS */|#
  ( #x07b3 #x03ae ) #|/*             Greek_etaaccent ή GREEK SMALL LETTER ETA WITH TONOS */|#
  ( #x07b4 #x03af ) #|/*            Greek_iotaaccent ί GREEK SMALL LETTER IOTA WITH TONOS */|#
  ( #x07b5 #x03ca ) #|/*          Greek_iotadieresis ϊ GREEK SMALL LETTER IOTA WITH DIALYTIKA */|#
  ( #x07b6 #x0390 ) #|/*    Greek_iotaaccentdieresis ΐ GREEK SMALL LETTER IOTA WITH DIALYTIKA AND TONOS */|#
  ( #x07b7 #x03cc ) #|/*         Greek_omicronaccent ό GREEK SMALL LETTER OMICRON WITH TONOS */|#
  ( #x07b8 #x03cd ) #|/*         Greek_upsilonaccent ύ GREEK SMALL LETTER UPSILON WITH TONOS */|#
  ( #x07b9 #x03cb ) #|/*       Greek_upsilondieresis ϋ GREEK SMALL LETTER UPSILON WITH DIALYTIKA */|#
  ( #x07ba #x03b0 ) #|/* Greek_upsilonaccentdieresis ΰ GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND TONOS */|#
  ( #x07bb #x03ce ) #|/*           Greek_omegaaccent ώ GREEK SMALL LETTER OMEGA WITH TONOS */|#
  ( #x07c1 #x0391 ) #|/*                 Greek_ALPHA Α GREEK CAPITAL LETTER ALPHA */|#
  ( #x07c2 #x0392 ) #|/*                  Greek_BETA Β GREEK CAPITAL LETTER BETA */|#
  ( #x07c3 #x0393 ) #|/*                 Greek_GAMMA Γ GREEK CAPITAL LETTER GAMMA */|#
  ( #x07c4 #x0394 ) #|/*                 Greek_DELTA Δ GREEK CAPITAL LETTER DELTA */|#
  ( #x07c5 #x0395 ) #|/*               Greek_EPSILON Ε GREEK CAPITAL LETTER EPSILON */|#
  ( #x07c6 #x0396 ) #|/*                  Greek_ZETA Ζ GREEK CAPITAL LETTER ZETA */|#
  ( #x07c7 #x0397 ) #|/*                   Greek_ETA Η GREEK CAPITAL LETTER ETA */|#
  ( #x07c8 #x0398 ) #|/*                 Greek_THETA Θ GREEK CAPITAL LETTER THETA */|#
  ( #x07c9 #x0399 ) #|/*                  Greek_IOTA Ι GREEK CAPITAL LETTER IOTA */|#
  ( #x07ca #x039a ) #|/*                 Greek_KAPPA Κ GREEK CAPITAL LETTER KAPPA */|#
  ( #x07cb #x039b ) #|/*                Greek_LAMBDA Λ GREEK CAPITAL LETTER LAMDA */|#
  ( #x07cc #x039c ) #|/*                    Greek_MU Μ GREEK CAPITAL LETTER MU */|#
  ( #x07cd #x039d ) #|/*                    Greek_NU Ν GREEK CAPITAL LETTER NU */|#
  ( #x07ce #x039e ) #|/*                    Greek_XI Ξ GREEK CAPITAL LETTER XI */|#
  ( #x07cf #x039f ) #|/*               Greek_OMICRON Ο GREEK CAPITAL LETTER OMICRON */|#
  ( #x07d0 #x03a0 ) #|/*                    Greek_PI Π GREEK CAPITAL LETTER PI */|#
  ( #x07d1 #x03a1 ) #|/*                   Greek_RHO Ρ GREEK CAPITAL LETTER RHO */|#
  ( #x07d2 #x03a3 ) #|/*                 Greek_SIGMA Σ GREEK CAPITAL LETTER SIGMA */|#
  ( #x07d4 #x03a4 ) #|/*                   Greek_TAU Τ GREEK CAPITAL LETTER TAU */|#
  ( #x07d5 #x03a5 ) #|/*               Greek_UPSILON Υ GREEK CAPITAL LETTER UPSILON */|#
  ( #x07d6 #x03a6 ) #|/*                   Greek_PHI Φ GREEK CAPITAL LETTER PHI */|#
  ( #x07d7 #x03a7 ) #|/*                   Greek_CHI Χ GREEK CAPITAL LETTER CHI */|#
  ( #x07d8 #x03a8 ) #|/*                   Greek_PSI Ψ GREEK CAPITAL LETTER PSI */|#
  ( #x07d9 #x03a9 ) #|/*                 Greek_OMEGA Ω GREEK CAPITAL LETTER OMEGA */|#
  ( #x07e1 #x03b1 ) #|/*                 Greek_alpha α GREEK SMALL LETTER ALPHA */|#
  ( #x07e2 #x03b2 ) #|/*                  Greek_beta β GREEK SMALL LETTER BETA */|#
  ( #x07e3 #x03b3 ) #|/*                 Greek_gamma γ GREEK SMALL LETTER GAMMA */|#
  ( #x07e4 #x03b4 ) #|/*                 Greek_delta δ GREEK SMALL LETTER DELTA */|#
  ( #x07e5 #x03b5 ) #|/*               Greek_epsilon ε GREEK SMALL LETTER EPSILON */|#
  ( #x07e6 #x03b6 ) #|/*                  Greek_zeta ζ GREEK SMALL LETTER ZETA */|#
  ( #x07e7 #x03b7 ) #|/*                   Greek_eta η GREEK SMALL LETTER ETA */|#
  ( #x07e8 #x03b8 ) #|/*                 Greek_theta θ GREEK SMALL LETTER THETA */|#
  ( #x07e9 #x03b9 ) #|/*                  Greek_iota ι GREEK SMALL LETTER IOTA */|#
  ( #x07ea #x03ba ) #|/*                 Greek_kappa κ GREEK SMALL LETTER KAPPA */|#
  ( #x07eb #x03bb ) #|/*                Greek_lambda λ GREEK SMALL LETTER LAMDA */|#
  ( #x07ec #x03bc ) #|/*                    Greek_mu μ GREEK SMALL LETTER MU */|#
  ( #x07ed #x03bd ) #|/*                    Greek_nu ν GREEK SMALL LETTER NU */|#
  ( #x07ee #x03be ) #|/*                    Greek_xi ξ GREEK SMALL LETTER XI */|#
  ( #x07ef #x03bf ) #|/*               Greek_omicron ο GREEK SMALL LETTER OMICRON */|#
  ( #x07f0 #x03c0 ) #|/*                    Greek_pi π GREEK SMALL LETTER PI */|#
  ( #x07f1 #x03c1 ) #|/*                   Greek_rho ρ GREEK SMALL LETTER RHO */|#
  ( #x07f2 #x03c3 ) #|/*                 Greek_sigma σ GREEK SMALL LETTER SIGMA */|#
  ( #x07f3 #x03c2 ) #|/*       Greek_finalsmallsigma ς GREEK SMALL LETTER FINAL SIGMA */|#
  ( #x07f4 #x03c4 ) #|/*                   Greek_tau τ GREEK SMALL LETTER TAU */|#
  ( #x07f5 #x03c5 ) #|/*               Greek_upsilon υ GREEK SMALL LETTER UPSILON */|#
  ( #x07f6 #x03c6 ) #|/*                   Greek_phi φ GREEK SMALL LETTER PHI */|#
  ( #x07f7 #x03c7 ) #|/*                   Greek_chi χ GREEK SMALL LETTER CHI */|#
  ( #x07f8 #x03c8 ) #|/*                   Greek_psi ψ GREEK SMALL LETTER PSI */|#
  ( #x07f9 #x03c9 ) #|/*                 Greek_omega ω GREEK SMALL LETTER OMEGA */|#
  ( #x08a1 #x23b7 ) #|/*                 leftradical ⎷ ??? */|#
  ( #x08a2 #x250c ) #|/*              topleftradical ┌ BOX DRAWINGS LIGHT DOWN AND RIGHT */|#
  ( #x08a3 #x2500 ) #|/*              horizconnector ─ BOX DRAWINGS LIGHT HORIZONTAL */|#
  ( #x08a4 #x2320 ) #|/*                 topintegral ⌠ TOP HALF INTEGRAL */|#
  ( #x08a5 #x2321 ) #|/*                 botintegral ⌡ BOTTOM HALF INTEGRAL */|#
  ( #x08a6 #x2502 ) #|/*               vertconnector │ BOX DRAWINGS LIGHT VERTICAL */|#
  ( #x08a7 #x23a1 ) #|/*            topleftsqbracket ⎡ ??? */|#
  ( #x08a8 #x23a3 ) #|/*            botleftsqbracket ⎣ ??? */|#
  ( #x08a9 #x23a4 ) #|/*           toprightsqbracket ⎤ ??? */|#
  ( #x08aa #x23a6 ) #|/*           botrightsqbracket ⎦ ??? */|#
  ( #x08ab #x239b ) #|/*               topleftparens ⎛ ??? */|#
  ( #x08ac #x239d ) #|/*               botleftparens ⎝ ??? */|#
  ( #x08ad #x239e ) #|/*              toprightparens ⎞ ??? */|#
  ( #x08ae #x23a0 ) #|/*              botrightparens ⎠ ??? */|#
  ( #x08af #x23a8 ) #|/*        leftmiddlecurlybrace ⎨ ??? */|#
  ( #x08b0 #x23ac ) #|/*       rightmiddlecurlybrace ⎬ ??? */|#
#|/*  #x08b1                          topleftsummation ? ??? */|#
#|/*  #x08b2                          botleftsummation ? ??? */|#
#|/*  #x08b3                 topvertsummationconnector ? ??? */|#
#|/*  #x08b4                 botvertsummationconnector ? ??? */|#
#|/*  #x08b5                         toprightsummation ? ??? */|#
#|/*  #x08b6                         botrightsummation ? ??? */|#
#|/*  #x08b7                      rightmiddlesummation ? ??? */|#
  ( #x08bc #x2264 ) #|/*               lessthanequal ≤ LESS-THAN OR EQUAL TO */|#
  ( #x08bd #x2260 ) #|/*                    notequal ≠ NOT EQUAL TO */|#
  ( #x08be #x2265 ) #|/*            greaterthanequal ≥ GREATER-THAN OR EQUAL TO */|#
  ( #x08bf #x222b ) #|/*                    integral ∫ INTEGRAL */|#
  ( #x08c0 #x2234 ) #|/*                   therefore ∴ THEREFORE */|#
  ( #x08c1 #x221d ) #|/*                   variation ∝ PROPORTIONAL TO */|#
  ( #x08c2 #x221e ) #|/*                    infinity ∞ INFINITY */|#
  ( #x08c5 #x2207 ) #|/*                       nabla ∇ NABLA */|#
  ( #x08c8 #x223c ) #|/*                 approximate ∼ TILDE OPERATOR */|#
  ( #x08c9 #x2243 ) #|/*                similarequal ≃ ASYMPTOTICALLY EQUAL TO */|#
  ( #x08cd #x21d4 ) #|/*                    ifonlyif ⇔ LEFT RIGHT DOUBLE ARROW */|#
  ( #x08ce #x21d2 ) #|/*                     implies ⇒ RIGHTWARDS DOUBLE ARROW */|#
  ( #x08cf #x2261 ) #|/*                   identical ≡ IDENTICAL TO */|#
  ( #x08d6 #x221a ) #|/*                     radical √ SQUARE ROOT */|#
  ( #x08da #x2282 ) #|/*                  includedin ⊂ SUBSET OF */|#
  ( #x08db #x2283 ) #|/*                    includes ⊃ SUPERSET OF */|#
  ( #x08dc #x2229 ) #|/*                intersection ∩ INTERSECTION */|#
  ( #x08dd #x222a ) #|/*                       union ∪ UNION */|#
  ( #x08de #x2227 ) #|/*                  logicaland ∧ LOGICAL AND */|#
  ( #x08df #x2228 ) #|/*                   logicalor ∨ LOGICAL OR */|#
  ( #x08ef #x2202 ) #|/*           partialderivative ∂ PARTIAL DIFFERENTIAL */|#
  ( #x08f6 #x0192 ) #|/*                    function ƒ LATIN SMALL LETTER F WITH HOOK */|#
  ( #x08fb #x2190 ) #|/*                   leftarrow ← LEFTWARDS ARROW */|#
  ( #x08fc #x2191 ) #|/*                     uparrow ↑ UPWARDS ARROW */|#
  ( #x08fd #x2192 ) #|/*                  rightarrow → RIGHTWARDS ARROW */|#
  ( #x08fe #x2193 ) #|/*                   downarrow ↓ DOWNWARDS ARROW */|#
#|/*  #x09df                                     blank ? ??? */|#
  ( #x09e0 #x25c6 ) #|/*                soliddiamond ◆ BLACK DIAMOND */|#
  ( #x09e1 #x2592 ) #|/*                checkerboard ▒ MEDIUM SHADE */|#
  ( #x09e2 #x2409 ) #|/*                          ht ␉ SYMBOL FOR HORIZONTAL TABULATION */|#
  ( #x09e3 #x240c ) #|/*                          ff ␌ SYMBOL FOR FORM FEED */|#
  ( #x09e4 #x240d ) #|/*                          cr ␍ SYMBOL FOR CARRIAGE RETURN */|#
  ( #x09e5 #x240a ) #|/*                          lf ␊ SYMBOL FOR LINE FEED */|#
  ( #x09e8 #x2424 ) #|/*                          nl ␤ SYMBOL FOR NEWLINE */|#
  ( #x09e9 #x240b ) #|/*                          vt ␋ SYMBOL FOR VERTICAL TABULATION */|#
  ( #x09ea #x2518 ) #|/*              lowrightcorner ┘ BOX DRAWINGS LIGHT UP AND LEFT */|#
  ( #x09eb #x2510 ) #|/*               uprightcorner ┐ BOX DRAWINGS LIGHT DOWN AND LEFT */|#
  ( #x09ec #x250c ) #|/*                upleftcorner ┌ BOX DRAWINGS LIGHT DOWN AND RIGHT */|#
  ( #x09ed #x2514 ) #|/*               lowleftcorner └ BOX DRAWINGS LIGHT UP AND RIGHT */|#
  ( #x09ee #x253c ) #|/*               crossinglines ┼ BOX DRAWINGS LIGHT VERTICAL AND HORIZONTAL */|#
  ( #x09ef #x23ba ) #|/*              horizlinescan1 ⎺ HORIZONTAL SCAN LINE-1 (Unicode 3.2 draft) */|#
  ( #x09f0 #x23bb ) #|/*              horizlinescan3 ⎻ HORIZONTAL SCAN LINE-3 (Unicode 3.2 draft) */|#
  ( #x09f1 #x2500 ) #|/*              horizlinescan5 ─ BOX DRAWINGS LIGHT HORIZONTAL */|#
  ( #x09f2 #x23bc ) #|/*              horizlinescan7 ⎼ HORIZONTAL SCAN LINE-7 (Unicode 3.2 draft) */|#
  ( #x09f3 #x23bd ) #|/*              horizlinescan9 ⎽ HORIZONTAL SCAN LINE-9 (Unicode 3.2 draft) */|#
  ( #x09f4 #x251c ) #|/*                       leftt ├ BOX DRAWINGS LIGHT VERTICAL AND RIGHT */|#
  ( #x09f5 #x2524 ) #|/*                      rightt ┤ BOX DRAWINGS LIGHT VERTICAL AND LEFT */|#
  ( #x09f6 #x2534 ) #|/*                        bott ┴ BOX DRAWINGS LIGHT UP AND HORIZONTAL */|#
  ( #x09f7 #x252c ) #|/*                        topt ┬ BOX DRAWINGS LIGHT DOWN AND HORIZONTAL */|#
  ( #x09f8 #x2502 ) #|/*                     vertbar │ BOX DRAWINGS LIGHT VERTICAL */|#
  ( #x0aa1 #x2003 ) #|/*                     emspace   EM SPACE */|#
  ( #x0aa2 #x2002 ) #|/*                     enspace   EN SPACE */|#
  ( #x0aa3 #x2004 ) #|/*                    em3space   THREE-PER-EM SPACE */|#
  ( #x0aa4 #x2005 ) #|/*                    em4space   FOUR-PER-EM SPACE */|#
  ( #x0aa5 #x2007 ) #|/*                  digitspace   FIGURE SPACE */|#
  ( #x0aa6 #x2008 ) #|/*                  punctspace   PUNCTUATION SPACE */|#
  ( #x0aa7 #x2009 ) #|/*                   thinspace   THIN SPACE */|#
  ( #x0aa8 #x200a ) #|/*                   hairspace   HAIR SPACE */|#
  ( #x0aa9 #x2014 ) #|/*                      emdash — EM DASH */|#
  ( #x0aaa #x2013 ) #|/*                      endash – EN DASH */|#
  ( #x0aac #x2423 ) #|/*                 signifblank ␣ OPEN BOX */|#
  ( #x0aae #x2026 ) #|/*                    ellipsis … HORIZONTAL ELLIPSIS */|#
  ( #x0aaf #x2025 ) #|/*             doubbaselinedot ‥ TWO DOT LEADER */|#
  ( #x0ab0 #x2153 ) #|/*                    onethird ⅓ VULGAR FRACTION ONE THIRD */|#
  ( #x0ab1 #x2154 ) #|/*                   twothirds ⅔ VULGAR FRACTION TWO THIRDS */|#
  ( #x0ab2 #x2155 ) #|/*                    onefifth ⅕ VULGAR FRACTION ONE FIFTH */|#
  ( #x0ab3 #x2156 ) #|/*                   twofifths ⅖ VULGAR FRACTION TWO FIFTHS */|#
  ( #x0ab4 #x2157 ) #|/*                 threefifths ⅗ VULGAR FRACTION THREE FIFTHS */|#
  ( #x0ab5 #x2158 ) #|/*                  fourfifths ⅘ VULGAR FRACTION FOUR FIFTHS */|#
  ( #x0ab6 #x2159 ) #|/*                    onesixth ⅙ VULGAR FRACTION ONE SIXTH */|#
  ( #x0ab7 #x215a ) #|/*                  fivesixths ⅚ VULGAR FRACTION FIVE SIXTHS */|#
  ( #x0ab8 #x2105 ) #|/*                      careof ℅ CARE OF */|#
  ( #x0abb #x2012 ) #|/*                     figdash ‒ FIGURE DASH */|#
  ( #x0abc #x27e8 ) #|/*            leftanglebracket ⟨ MATHEMATICAL LEFT ANGLE BRACKET */|#
  ( #x0abd #x002e ) #|/*                decimalpoint . FULL STOP */|#
  ( #x0abe #x27e9 ) #|/*           rightanglebracket ⟩ MATHEMATICAL RIGHT ANGLE BRACKET */|#
#|/*  #x0abf                                    marker ? ??? */|#
  ( #x0ac3 #x215b ) #|/*                   oneeighth ⅛ VULGAR FRACTION ONE EIGHTH */|#
  ( #x0ac4 #x215c ) #|/*                threeeighths ⅜ VULGAR FRACTION THREE EIGHTHS */|#
  ( #x0ac5 #x215d ) #|/*                 fiveeighths ⅝ VULGAR FRACTION FIVE EIGHTHS */|#
  ( #x0ac6 #x215e ) #|/*                seveneighths ⅞ VULGAR FRACTION SEVEN EIGHTHS */|#
  ( #x0ac9 #x2122 ) #|/*                   trademark ™ TRADE MARK SIGN */|#
  ( #x0aca #x2613 ) #|/*               signaturemark ☓ SALTIRE */|#
#|/*  #x0acb                         trademarkincircle ? ??? */|#
  ( #x0acc #x25c1 ) #|/*            leftopentriangle ◁ WHITE LEFT-POINTING TRIANGLE */|#
  ( #x0acd #x25b7 ) #|/*           rightopentriangle ▷ WHITE RIGHT-POINTING TRIANGLE */|#
  ( #x0ace #x25cb ) #|/*                emopencircle ○ WHITE CIRCLE */|#
  ( #x0acf #x25af ) #|/*             emopenrectangle ▯ WHITE VERTICAL RECTANGLE */|#
  ( #x0ad0 #x2018 ) #|/*         leftsinglequotemark ‘ LEFT SINGLE QUOTATION MARK */|#
  ( #x0ad1 #x2019 ) #|/*        rightsinglequotemark ’ RIGHT SINGLE QUOTATION MARK */|#
  ( #x0ad2 #x201c ) #|/*         leftdoublequotemark “ LEFT DOUBLE QUOTATION MARK */|#
  ( #x0ad3 #x201d ) #|/*        rightdoublequotemark ” RIGHT DOUBLE QUOTATION MARK */|#
  ( #x0ad4 #x211e ) #|/*                prescription ℞ PRESCRIPTION TAKE */|#
  ( #x0ad5 #x2030 ) #|/*                    permille ‰ PER MILLE SIGN */|#
  ( #x0ad6 #x2032 ) #|/*                     minutes ′ PRIME */|#
  ( #x0ad7 #x2033 ) #|/*                     seconds ″ DOUBLE PRIME */|#
  ( #x0ad9 #x271d ) #|/*                  latincross ✝ LATIN CROSS */|#
#|/*  #x0ada                                  hexagram ? ??? */|#
  ( #x0adb #x25ac ) #|/*            filledrectbullet ▬ BLACK RECTANGLE */|#
  ( #x0adc #x25c0 ) #|/*         filledlefttribullet ◀ BLACK LEFT-POINTING TRIANGLE */|#
  ( #x0add #x25b6 ) #|/*        filledrighttribullet ▶ BLACK RIGHT-POINTING TRIANGLE */|#
  ( #x0ade #x25cf ) #|/*              emfilledcircle ● BLACK CIRCLE */|#
  ( #x0adf #x25ae ) #|/*                emfilledrect ▮ BLACK VERTICAL RECTANGLE */|#
  ( #x0ae0 #x25e6 ) #|/*            enopencircbullet ◦ WHITE BULLET */|#
  ( #x0ae1 #x25ab ) #|/*          enopensquarebullet ▫ WHITE SMALL SQUARE */|#
  ( #x0ae2 #x25ad ) #|/*              openrectbullet ▭ WHITE RECTANGLE */|#
  ( #x0ae3 #x25b3 ) #|/*             opentribulletup △ WHITE UP-POINTING TRIANGLE */|#
  ( #x0ae4 #x25bd ) #|/*           opentribulletdown ▽ WHITE DOWN-POINTING TRIANGLE */|#
  ( #x0ae5 #x2606 ) #|/*                    openstar ☆ WHITE STAR */|#
  ( #x0ae6 #x2022 ) #|/*          enfilledcircbullet • BULLET */|#
  ( #x0ae7 #x25aa ) #|/*            enfilledsqbullet ▪ BLACK SMALL SQUARE */|#
  ( #x0ae8 #x25b2 ) #|/*           filledtribulletup ▲ BLACK UP-POINTING TRIANGLE */|#
  ( #x0ae9 #x25bc ) #|/*         filledtribulletdown ▼ BLACK DOWN-POINTING TRIANGLE */|#
  ( #x0aea #x261c ) #|/*                 leftpointer ☜ WHITE LEFT POINTING INDEX */|#
  ( #x0aeb #x261e ) #|/*                rightpointer ☞ WHITE RIGHT POINTING INDEX */|#
  ( #x0aec #x2663 ) #|/*                        club ♣ BLACK CLUB SUIT */|#
  ( #x0aed #x2666 ) #|/*                     diamond ♦ BLACK DIAMOND SUIT */|#
  ( #x0aee #x2665 ) #|/*                       heart ♥ BLACK HEART SUIT */|#
  ( #x0af0 #x2720 ) #|/*                maltesecross ✠ MALTESE CROSS */|#
  ( #x0af1 #x2020 ) #|/*                      dagger † DAGGER */|#
  ( #x0af2 #x2021 ) #|/*                doubledagger ‡ DOUBLE DAGGER */|#
  ( #x0af3 #x2713 ) #|/*                   checkmark ✓ CHECK MARK */|#
  ( #x0af4 #x2717 ) #|/*                 ballotcross ✗ BALLOT X */|#
  ( #x0af5 #x266f ) #|/*                musicalsharp ♯ MUSIC SHARP SIGN */|#
  ( #x0af6 #x266d ) #|/*                 musicalflat ♭ MUSIC FLAT SIGN */|#
  ( #x0af7 #x2642 ) #|/*                  malesymbol ♂ MALE SIGN */|#
  ( #x0af8 #x2640 ) #|/*                femalesymbol ♀ FEMALE SIGN */|#
  ( #x0af9 #x260e ) #|/*                   telephone ☎ BLACK TELEPHONE */|#
  ( #x0afa #x2315 ) #|/*           telephonerecorder ⌕ TELEPHONE RECORDER */|#
  ( #x0afb #x2117 ) #|/*         phonographcopyright ℗ SOUND RECORDING COPYRIGHT */|#
  ( #x0afc #x2038 ) #|/*                       caret ‸ CARET */|#
  ( #x0afd #x201a ) #|/*          singlelowquotemark ‚ SINGLE LOW-9 QUOTATION MARK */|#
  ( #x0afe #x201e ) #|/*          doublelowquotemark „ DOUBLE LOW-9 QUOTATION MARK */|#
#|/*  #x0aff                                    cursor ? ??? */|#
  ( #x0ba3 #x003c ) #|/*                   leftcaret < LESS-THAN SIGN */|#
  ( #x0ba6 #x003e ) #|/*                  rightcaret > GREATER-THAN SIGN */|#
  ( #x0ba8 #x2228 ) #|/*                   downcaret ∨ LOGICAL OR */|#
  ( #x0ba9 #x2227 ) #|/*                     upcaret ∧ LOGICAL AND */|#
  ( #x0bc0 #x00af ) #|/*                     overbar ¯ MACRON */|#
  ( #x0bc2 #x22a4 ) #|/*                    downtack ⊤ DOWN TACK */|#
  ( #x0bc3 #x2229 ) #|/*                      upshoe ∩ INTERSECTION */|#
  ( #x0bc4 #x230a ) #|/*                   downstile ⌊ LEFT FLOOR */|#
  ( #x0bc6 #x005f ) #|/*                    underbar _ LOW LINE */|#
  ( #x0bca #x2218 ) #|/*                         jot ∘ RING OPERATOR */|#
  ( #x0bcc #x2395 ) #|/*                        quad ⎕ APL FUNCTIONAL SYMBOL QUAD (Unicode 3.0) */|#
  ( #x0bce #x22a5 ) #|/*                      uptack ⊥ UP TACK */|#
  ( #x0bcf #x25cb ) #|/*                      circle ○ WHITE CIRCLE */|#
  ( #x0bd3 #x2308 ) #|/*                     upstile ⌈ LEFT CEILING */|#
  ( #x0bd6 #x222a ) #|/*                    downshoe ∪ UNION */|#
  ( #x0bd8 #x2283 ) #|/*                   rightshoe ⊃ SUPERSET OF */|#
  ( #x0bda #x2282 ) #|/*                    leftshoe ⊂ SUBSET OF */|#
  ( #x0bdc #x22a3 ) #|/*                    lefttack ⊣ LEFT TACK */|#
  ( #x0bfc #x22a2 ) #|/*                   righttack ⊢ RIGHT TACK */|#
  ( #x0cdf #x2017 ) #|/*        hebrew_doublelowline ‗ DOUBLE LOW LINE */|#
  ( #x0ce0 #x05d0 ) #|/*                hebrew_aleph א HEBREW LETTER ALEF */|#
  ( #x0ce1 #x05d1 ) #|/*                  hebrew_bet ב HEBREW LETTER BET */|#
  ( #x0ce2 #x05d2 ) #|/*                hebrew_gimel ג HEBREW LETTER GIMEL */|#
  ( #x0ce3 #x05d3 ) #|/*                hebrew_dalet ד HEBREW LETTER DALET */|#
  ( #x0ce4 #x05d4 ) #|/*                   hebrew_he ה HEBREW LETTER HE */|#
  ( #x0ce5 #x05d5 ) #|/*                  hebrew_waw ו HEBREW LETTER VAV */|#
  ( #x0ce6 #x05d6 ) #|/*                 hebrew_zain ז HEBREW LETTER ZAYIN */|#
  ( #x0ce7 #x05d7 ) #|/*                 hebrew_chet ח HEBREW LETTER HET */|#
  ( #x0ce8 #x05d8 ) #|/*                  hebrew_tet ט HEBREW LETTER TET */|#
  ( #x0ce9 #x05d9 ) #|/*                  hebrew_yod י HEBREW LETTER YOD */|#
  ( #x0cea #x05da ) #|/*            hebrew_finalkaph ך HEBREW LETTER FINAL KAF */|#
  ( #x0ceb #x05db ) #|/*                 hebrew_kaph כ HEBREW LETTER KAF */|#
  ( #x0cec #x05dc ) #|/*                hebrew_lamed ל HEBREW LETTER LAMED */|#
  ( #x0ced #x05dd ) #|/*             hebrew_finalmem ם HEBREW LETTER FINAL MEM */|#
  ( #x0cee #x05de ) #|/*                  hebrew_mem מ HEBREW LETTER MEM */|#
  ( #x0cef #x05df ) #|/*             hebrew_finalnun ן HEBREW LETTER FINAL NUN */|#
  ( #x0cf0 #x05e0 ) #|/*                  hebrew_nun נ HEBREW LETTER NUN */|#
  ( #x0cf1 #x05e1 ) #|/*               hebrew_samech ס HEBREW LETTER SAMEKH */|#
  ( #x0cf2 #x05e2 ) #|/*                 hebrew_ayin ע HEBREW LETTER AYIN */|#
  ( #x0cf3 #x05e3 ) #|/*              hebrew_finalpe ף HEBREW LETTER FINAL PE */|#
  ( #x0cf4 #x05e4 ) #|/*                   hebrew_pe פ HEBREW LETTER PE */|#
  ( #x0cf5 #x05e5 ) #|/*            hebrew_finalzade ץ HEBREW LETTER FINAL TSADI */|#
  ( #x0cf6 #x05e6 ) #|/*                 hebrew_zade צ HEBREW LETTER TSADI */|#
  ( #x0cf7 #x05e7 ) #|/*                 hebrew_qoph ק HEBREW LETTER QOF */|#
  ( #x0cf8 #x05e8 ) #|/*                 hebrew_resh ר HEBREW LETTER RESH */|#
  ( #x0cf9 #x05e9 ) #|/*                 hebrew_shin ש HEBREW LETTER SHIN */|#
  ( #x0cfa #x05ea ) #|/*                  hebrew_taw ת HEBREW LETTER TAV */|#
  ( #x0da1 #x0e01 ) #|/*                  Thai_kokai ก THAI CHARACTER KO KAI */|#
  ( #x0da2 #x0e02 ) #|/*                Thai_khokhai ข THAI CHARACTER KHO KHAI */|#
  ( #x0da3 #x0e03 ) #|/*               Thai_khokhuat ฃ THAI CHARACTER KHO KHUAT */|#
  ( #x0da4 #x0e04 ) #|/*               Thai_khokhwai ค THAI CHARACTER KHO KHWAI */|#
  ( #x0da5 #x0e05 ) #|/*                Thai_khokhon ฅ THAI CHARACTER KHO KHON */|#
  ( #x0da6 #x0e06 ) #|/*             Thai_khorakhang ฆ THAI CHARACTER KHO RAKHANG */|#
  ( #x0da7 #x0e07 ) #|/*                 Thai_ngongu ง THAI CHARACTER NGO NGU */|#
  ( #x0da8 #x0e08 ) #|/*                Thai_chochan จ THAI CHARACTER CHO CHAN */|#
  ( #x0da9 #x0e09 ) #|/*               Thai_choching ฉ THAI CHARACTER CHO CHING */|#
  ( #x0daa #x0e0a ) #|/*               Thai_chochang ช THAI CHARACTER CHO CHANG */|#
  ( #x0dab #x0e0b ) #|/*                   Thai_soso ซ THAI CHARACTER SO SO */|#
  ( #x0dac #x0e0c ) #|/*                Thai_chochoe ฌ THAI CHARACTER CHO CHOE */|#
  ( #x0dad #x0e0d ) #|/*                 Thai_yoying ญ THAI CHARACTER YO YING */|#
  ( #x0dae #x0e0e ) #|/*                Thai_dochada ฎ THAI CHARACTER DO CHADA */|#
  ( #x0daf #x0e0f ) #|/*                Thai_topatak ฏ THAI CHARACTER TO PATAK */|#
  ( #x0db0 #x0e10 ) #|/*                Thai_thothan ฐ THAI CHARACTER THO THAN */|#
  ( #x0db1 #x0e11 ) #|/*          Thai_thonangmontho ฑ THAI CHARACTER THO NANGMONTHO */|#
  ( #x0db2 #x0e12 ) #|/*             Thai_thophuthao ฒ THAI CHARACTER THO PHUTHAO */|#
  ( #x0db3 #x0e13 ) #|/*                  Thai_nonen ณ THAI CHARACTER NO NEN */|#
  ( #x0db4 #x0e14 ) #|/*                  Thai_dodek ด THAI CHARACTER DO DEK */|#
  ( #x0db5 #x0e15 ) #|/*                  Thai_totao ต THAI CHARACTER TO TAO */|#
  ( #x0db6 #x0e16 ) #|/*               Thai_thothung ถ THAI CHARACTER THO THUNG */|#
  ( #x0db7 #x0e17 ) #|/*              Thai_thothahan ท THAI CHARACTER THO THAHAN */|#
  ( #x0db8 #x0e18 ) #|/*               Thai_thothong ธ THAI CHARACTER THO THONG */|#
  ( #x0db9 #x0e19 ) #|/*                   Thai_nonu น THAI CHARACTER NO NU */|#
  ( #x0dba #x0e1a ) #|/*               Thai_bobaimai บ THAI CHARACTER BO BAIMAI */|#
  ( #x0dbb #x0e1b ) #|/*                  Thai_popla ป THAI CHARACTER PO PLA */|#
  ( #x0dbc #x0e1c ) #|/*               Thai_phophung ผ THAI CHARACTER PHO PHUNG */|#
  ( #x0dbd #x0e1d ) #|/*                   Thai_fofa ฝ THAI CHARACTER FO FA */|#
  ( #x0dbe #x0e1e ) #|/*                Thai_phophan พ THAI CHARACTER PHO PHAN */|#
  ( #x0dbf #x0e1f ) #|/*                  Thai_fofan ฟ THAI CHARACTER FO FAN */|#
  ( #x0dc0 #x0e20 ) #|/*             Thai_phosamphao ภ THAI CHARACTER PHO SAMPHAO */|#
  ( #x0dc1 #x0e21 ) #|/*                   Thai_moma ม THAI CHARACTER MO MA */|#
  ( #x0dc2 #x0e22 ) #|/*                  Thai_yoyak ย THAI CHARACTER YO YAK */|#
  ( #x0dc3 #x0e23 ) #|/*                  Thai_rorua ร THAI CHARACTER RO RUA */|#
  ( #x0dc4 #x0e24 ) #|/*                     Thai_ru ฤ THAI CHARACTER RU */|#
  ( #x0dc5 #x0e25 ) #|/*                 Thai_loling ล THAI CHARACTER LO LING */|#
  ( #x0dc6 #x0e26 ) #|/*                     Thai_lu ฦ THAI CHARACTER LU */|#
  ( #x0dc7 #x0e27 ) #|/*                 Thai_wowaen ว THAI CHARACTER WO WAEN */|#
  ( #x0dc8 #x0e28 ) #|/*                 Thai_sosala ศ THAI CHARACTER SO SALA */|#
  ( #x0dc9 #x0e29 ) #|/*                 Thai_sorusi ษ THAI CHARACTER SO RUSI */|#
  ( #x0dca #x0e2a ) #|/*                  Thai_sosua ส THAI CHARACTER SO SUA */|#
  ( #x0dcb #x0e2b ) #|/*                  Thai_hohip ห THAI CHARACTER HO HIP */|#
  ( #x0dcc #x0e2c ) #|/*                Thai_lochula ฬ THAI CHARACTER LO CHULA */|#
  ( #x0dcd #x0e2d ) #|/*                   Thai_oang อ THAI CHARACTER O ANG */|#
  ( #x0dce #x0e2e ) #|/*               Thai_honokhuk ฮ THAI CHARACTER HO NOKHUK */|#
  ( #x0dcf #x0e2f ) #|/*              Thai_paiyannoi ฯ THAI CHARACTER PAIYANNOI */|#
  ( #x0dd0 #x0e30 ) #|/*                  Thai_saraa ะ THAI CHARACTER SARA A */|#
  ( #x0dd1 #x0e31 ) #|/*             Thai_maihanakat ั THAI CHARACTER MAI HAN-AKAT */|#
  ( #x0dd2 #x0e32 ) #|/*                 Thai_saraaa า THAI CHARACTER SARA AA */|#
  ( #x0dd3 #x0e33 ) #|/*                 Thai_saraam ำ THAI CHARACTER SARA AM */|#
  ( #x0dd4 #x0e34 ) #|/*                  Thai_sarai ิ THAI CHARACTER SARA I */|#
  ( #x0dd5 #x0e35 ) #|/*                 Thai_saraii ี THAI CHARACTER SARA II */|#
  ( #x0dd6 #x0e36 ) #|/*                 Thai_saraue ึ THAI CHARACTER SARA UE */|#
  ( #x0dd7 #x0e37 ) #|/*                Thai_sarauee ื THAI CHARACTER SARA UEE */|#
  ( #x0dd8 #x0e38 ) #|/*                  Thai_sarau ุ THAI CHARACTER SARA U */|#
  ( #x0dd9 #x0e39 ) #|/*                 Thai_sarauu ู THAI CHARACTER SARA UU */|#
  ( #x0dda #x0e3a ) #|/*                Thai_phinthu ฺ THAI CHARACTER PHINTHU */|#
  ( #x0dde #x0e3e ) #|/*      Thai_maihanakat_maitho ฾ ??? */|#
  ( #x0ddf #x0e3f ) #|/*                   Thai_baht ฿ THAI CURRENCY SYMBOL BAHT */|#
  ( #x0de0 #x0e40 ) #|/*                  Thai_sarae เ THAI CHARACTER SARA E */|#
  ( #x0de1 #x0e41 ) #|/*                 Thai_saraae แ THAI CHARACTER SARA AE */|#
  ( #x0de2 #x0e42 ) #|/*                  Thai_sarao โ THAI CHARACTER SARA O */|#
  ( #x0de3 #x0e43 ) #|/*          Thai_saraaimaimuan ใ THAI CHARACTER SARA AI MAIMUAN */|#
  ( #x0de4 #x0e44 ) #|/*         Thai_saraaimaimalai ไ THAI CHARACTER SARA AI MAIMALAI */|#
  ( #x0de5 #x0e45 ) #|/*            Thai_lakkhangyao ๅ THAI CHARACTER LAKKHANGYAO */|#
  ( #x0de6 #x0e46 ) #|/*               Thai_maiyamok ๆ THAI CHARACTER MAIYAMOK */|#
  ( #x0de7 #x0e47 ) #|/*              Thai_maitaikhu ็ THAI CHARACTER MAITAIKHU */|#
  ( #x0de8 #x0e48 ) #|/*                  Thai_maiek ่ THAI CHARACTER MAI EK */|#
  ( #x0de9 #x0e49 ) #|/*                 Thai_maitho ้ THAI CHARACTER MAI THO */|#
  ( #x0dea #x0e4a ) #|/*                 Thai_maitri ๊ THAI CHARACTER MAI TRI */|#
  ( #x0deb #x0e4b ) #|/*            Thai_maichattawa ๋ THAI CHARACTER MAI CHATTAWA */|#
  ( #x0dec #x0e4c ) #|/*            Thai_thanthakhat ์ THAI CHARACTER THANTHAKHAT */|#
  ( #x0ded #x0e4d ) #|/*               Thai_nikhahit ํ THAI CHARACTER NIKHAHIT */|#
  ( #x0df0 #x0e50 ) #|/*                 Thai_leksun ๐ THAI DIGIT ZERO */|#
  ( #x0df1 #x0e51 ) #|/*                Thai_leknung ๑ THAI DIGIT ONE */|#
  ( #x0df2 #x0e52 ) #|/*                Thai_leksong ๒ THAI DIGIT TWO */|#
  ( #x0df3 #x0e53 ) #|/*                 Thai_leksam ๓ THAI DIGIT THREE */|#
  ( #x0df4 #x0e54 ) #|/*                  Thai_leksi ๔ THAI DIGIT FOUR */|#
  ( #x0df5 #x0e55 ) #|/*                  Thai_lekha ๕ THAI DIGIT FIVE */|#
  ( #x0df6 #x0e56 ) #|/*                 Thai_lekhok ๖ THAI DIGIT SIX */|#
  ( #x0df7 #x0e57 ) #|/*                Thai_lekchet ๗ THAI DIGIT SEVEN */|#
  ( #x0df8 #x0e58 ) #|/*                Thai_lekpaet ๘ THAI DIGIT EIGHT */|#
  ( #x0df9 #x0e59 ) #|/*                 Thai_lekkao ๙ THAI DIGIT NINE */|#
  ( #x0ea1 #x3131 ) #|/*               Hangul_Kiyeog ㄱ HANGUL LETTER KIYEOK */|#
  ( #x0ea2 #x3132 ) #|/*          Hangul_SsangKiyeog ㄲ HANGUL LETTER SSANGKIYEOK */|#
  ( #x0ea3 #x3133 ) #|/*           Hangul_KiyeogSios ㄳ HANGUL LETTER KIYEOK-SIOS */|#
  ( #x0ea4 #x3134 ) #|/*                Hangul_Nieun ㄴ HANGUL LETTER NIEUN */|#
  ( #x0ea5 #x3135 ) #|/*           Hangul_NieunJieuj ㄵ HANGUL LETTER NIEUN-CIEUC */|#
  ( #x0ea6 #x3136 ) #|/*           Hangul_NieunHieuh ㄶ HANGUL LETTER NIEUN-HIEUH */|#
  ( #x0ea7 #x3137 ) #|/*               Hangul_Dikeud ㄷ HANGUL LETTER TIKEUT */|#
  ( #x0ea8 #x3138 ) #|/*          Hangul_SsangDikeud ㄸ HANGUL LETTER SSANGTIKEUT */|#
  ( #x0ea9 #x3139 ) #|/*                Hangul_Rieul ㄹ HANGUL LETTER RIEUL */|#
  ( #x0eaa #x313a ) #|/*          Hangul_RieulKiyeog ㄺ HANGUL LETTER RIEUL-KIYEOK */|#
  ( #x0eab #x313b ) #|/*           Hangul_RieulMieum ㄻ HANGUL LETTER RIEUL-MIEUM */|#
  ( #x0eac #x313c ) #|/*           Hangul_RieulPieub ㄼ HANGUL LETTER RIEUL-PIEUP */|#
  ( #x0ead #x313d ) #|/*            Hangul_RieulSios ㄽ HANGUL LETTER RIEUL-SIOS */|#
  ( #x0eae #x313e ) #|/*           Hangul_RieulTieut ㄾ HANGUL LETTER RIEUL-THIEUTH */|#
  ( #x0eaf #x313f ) #|/*          Hangul_RieulPhieuf ㄿ HANGUL LETTER RIEUL-PHIEUPH */|#
  ( #x0eb0 #x3140 ) #|/*           Hangul_RieulHieuh ㅀ HANGUL LETTER RIEUL-HIEUH */|#
  ( #x0eb1 #x3141 ) #|/*                Hangul_Mieum ㅁ HANGUL LETTER MIEUM */|#
  ( #x0eb2 #x3142 ) #|/*                Hangul_Pieub ㅂ HANGUL LETTER PIEUP */|#
  ( #x0eb3 #x3143 ) #|/*           Hangul_SsangPieub ㅃ HANGUL LETTER SSANGPIEUP */|#
  ( #x0eb4 #x3144 ) #|/*            Hangul_PieubSios ㅄ HANGUL LETTER PIEUP-SIOS */|#
  ( #x0eb5 #x3145 ) #|/*                 Hangul_Sios ㅅ HANGUL LETTER SIOS */|#
  ( #x0eb6 #x3146 ) #|/*            Hangul_SsangSios ㅆ HANGUL LETTER SSANGSIOS */|#
  ( #x0eb7 #x3147 ) #|/*                Hangul_Ieung ㅇ HANGUL LETTER IEUNG */|#
  ( #x0eb8 #x3148 ) #|/*                Hangul_Jieuj ㅈ HANGUL LETTER CIEUC */|#
  ( #x0eb9 #x3149 ) #|/*           Hangul_SsangJieuj ㅉ HANGUL LETTER SSANGCIEUC */|#
  ( #x0eba #x314a ) #|/*                Hangul_Cieuc ㅊ HANGUL LETTER CHIEUCH */|#
  ( #x0ebb #x314b ) #|/*               Hangul_Khieuq ㅋ HANGUL LETTER KHIEUKH */|#
  ( #x0ebc #x314c ) #|/*                Hangul_Tieut ㅌ HANGUL LETTER THIEUTH */|#
  ( #x0ebd #x314d ) #|/*               Hangul_Phieuf ㅍ HANGUL LETTER PHIEUPH */|#
  ( #x0ebe #x314e ) #|/*                Hangul_Hieuh ㅎ HANGUL LETTER HIEUH */|#
  ( #x0ebf #x314f ) #|/*                    Hangul_A ㅏ HANGUL LETTER A */|#
  ( #x0ec0 #x3150 ) #|/*                   Hangul_AE ㅐ HANGUL LETTER AE */|#
  ( #x0ec1 #x3151 ) #|/*                   Hangul_YA ㅑ HANGUL LETTER YA */|#
  ( #x0ec2 #x3152 ) #|/*                  Hangul_YAE ㅒ HANGUL LETTER YAE */|#
  ( #x0ec3 #x3153 ) #|/*                   Hangul_EO ㅓ HANGUL LETTER EO */|#
  ( #x0ec4 #x3154 ) #|/*                    Hangul_E ㅔ HANGUL LETTER E */|#
  ( #x0ec5 #x3155 ) #|/*                  Hangul_YEO ㅕ HANGUL LETTER YEO */|#
  ( #x0ec6 #x3156 ) #|/*                   Hangul_YE ㅖ HANGUL LETTER YE */|#
  ( #x0ec7 #x3157 ) #|/*                    Hangul_O ㅗ HANGUL LETTER O */|#
  ( #x0ec8 #x3158 ) #|/*                   Hangul_WA ㅘ HANGUL LETTER WA */|#
  ( #x0ec9 #x3159 ) #|/*                  Hangul_WAE ㅙ HANGUL LETTER WAE */|#
  ( #x0eca #x315a ) #|/*                   Hangul_OE ㅚ HANGUL LETTER OE */|#
  ( #x0ecb #x315b ) #|/*                   Hangul_YO ㅛ HANGUL LETTER YO */|#
  ( #x0ecc #x315c ) #|/*                    Hangul_U ㅜ HANGUL LETTER U */|#
  ( #x0ecd #x315d ) #|/*                  Hangul_WEO ㅝ HANGUL LETTER WEO */|#
  ( #x0ece #x315e ) #|/*                   Hangul_WE ㅞ HANGUL LETTER WE */|#
  ( #x0ecf #x315f ) #|/*                   Hangul_WI ㅟ HANGUL LETTER WI */|#
  ( #x0ed0 #x3160 ) #|/*                   Hangul_YU ㅠ HANGUL LETTER YU */|#
  ( #x0ed1 #x3161 ) #|/*                   Hangul_EU ㅡ HANGUL LETTER EU */|#
  ( #x0ed2 #x3162 ) #|/*                   Hangul_YI ㅢ HANGUL LETTER YI */|#
  ( #x0ed3 #x3163 ) #|/*                    Hangul_I ㅣ HANGUL LETTER I */|#
  ( #x0ed4 #x11a8 ) #|/*             Hangul_J_Kiyeog ᆨ HANGUL JONGSEONG KIYEOK */|#
  ( #x0ed5 #x11a9 ) #|/*        Hangul_J_SsangKiyeog ᆩ HANGUL JONGSEONG SSANGKIYEOK */|#
  ( #x0ed6 #x11aa ) #|/*         Hangul_J_KiyeogSios ᆪ HANGUL JONGSEONG KIYEOK-SIOS */|#
  ( #x0ed7 #x11ab ) #|/*              Hangul_J_Nieun ᆫ HANGUL JONGSEONG NIEUN */|#
  ( #x0ed8 #x11ac ) #|/*         Hangul_J_NieunJieuj ᆬ HANGUL JONGSEONG NIEUN-CIEUC */|#
  ( #x0ed9 #x11ad ) #|/*         Hangul_J_NieunHieuh ᆭ HANGUL JONGSEONG NIEUN-HIEUH */|#
  ( #x0eda #x11ae ) #|/*             Hangul_J_Dikeud ᆮ HANGUL JONGSEONG TIKEUT */|#
  ( #x0edb #x11af ) #|/*              Hangul_J_Rieul ᆯ HANGUL JONGSEONG RIEUL */|#
  ( #x0edc #x11b0 ) #|/*        Hangul_J_RieulKiyeog ᆰ HANGUL JONGSEONG RIEUL-KIYEOK */|#
  ( #x0edd #x11b1 ) #|/*         Hangul_J_RieulMieum ᆱ HANGUL JONGSEONG RIEUL-MIEUM */|#
  ( #x0ede #x11b2 ) #|/*         Hangul_J_RieulPieub ᆲ HANGUL JONGSEONG RIEUL-PIEUP */|#
  ( #x0edf #x11b3 ) #|/*          Hangul_J_RieulSios ᆳ HANGUL JONGSEONG RIEUL-SIOS */|#
  ( #x0ee0 #x11b4 ) #|/*         Hangul_J_RieulTieut ᆴ HANGUL JONGSEONG RIEUL-THIEUTH */|#
  ( #x0ee1 #x11b5 ) #|/*        Hangul_J_RieulPhieuf ᆵ HANGUL JONGSEONG RIEUL-PHIEUPH */|#
  ( #x0ee2 #x11b6 ) #|/*         Hangul_J_RieulHieuh ᆶ HANGUL JONGSEONG RIEUL-HIEUH */|#
  ( #x0ee3 #x11b7 ) #|/*              Hangul_J_Mieum ᆷ HANGUL JONGSEONG MIEUM */|#
  ( #x0ee4 #x11b8 ) #|/*              Hangul_J_Pieub ᆸ HANGUL JONGSEONG PIEUP */|#
  ( #x0ee5 #x11b9 ) #|/*          Hangul_J_PieubSios ᆹ HANGUL JONGSEONG PIEUP-SIOS */|#
  ( #x0ee6 #x11ba ) #|/*               Hangul_J_Sios ᆺ HANGUL JONGSEONG SIOS */|#
  ( #x0ee7 #x11bb ) #|/*          Hangul_J_SsangSios ᆻ HANGUL JONGSEONG SSANGSIOS */|#
  ( #x0ee8 #x11bc ) #|/*              Hangul_J_Ieung ᆼ HANGUL JONGSEONG IEUNG */|#
  ( #x0ee9 #x11bd ) #|/*              Hangul_J_Jieuj ᆽ HANGUL JONGSEONG CIEUC */|#
  ( #x0eea #x11be ) #|/*              Hangul_J_Cieuc ᆾ HANGUL JONGSEONG CHIEUCH */|#
  ( #x0eeb #x11bf ) #|/*             Hangul_J_Khieuq ᆿ HANGUL JONGSEONG KHIEUKH */|#
  ( #x0eec #x11c0 ) #|/*              Hangul_J_Tieut ᇀ HANGUL JONGSEONG THIEUTH */|#
  ( #x0eed #x11c1 ) #|/*             Hangul_J_Phieuf ᇁ HANGUL JONGSEONG PHIEUPH */|#
  ( #x0eee #x11c2 ) #|/*              Hangul_J_Hieuh ᇂ HANGUL JONGSEONG HIEUH */|#
  ( #x0eef #x316d ) #|/*     Hangul_RieulYeorinHieuh ㅭ HANGUL LETTER RIEUL-YEORINHIEUH */|#
  ( #x0ef0 #x3171 ) #|/*    Hangul_SunkyeongeumMieum ㅱ HANGUL LETTER KAPYEOUNMIEUM */|#
  ( #x0ef1 #x3178 ) #|/*    Hangul_SunkyeongeumPieub ㅸ HANGUL LETTER KAPYEOUNPIEUP */|#
  ( #x0ef2 #x317f ) #|/*              Hangul_PanSios ㅿ HANGUL LETTER PANSIOS */|#
  ( #x0ef3 #x3181 ) #|/*    Hangul_KkogjiDalrinIeung ㆁ HANGUL LETTER YESIEUNG */|#
  ( #x0ef4 #x3184 ) #|/*   Hangul_SunkyeongeumPhieuf ㆄ HANGUL LETTER KAPYEOUNPHIEUPH */|#
  ( #x0ef5 #x3186 ) #|/*          Hangul_YeorinHieuh ㆆ HANGUL LETTER YEORINHIEUH */|#
  ( #x0ef6 #x318d ) #|/*                Hangul_AraeA ㆍ HANGUL LETTER ARAEA */|#
  ( #x0ef7 #x318e ) #|/*               Hangul_AraeAE ㆎ HANGUL LETTER ARAEAE */|#
  ( #x0ef8 #x11eb ) #|/*            Hangul_J_PanSios ᇫ HANGUL JONGSEONG PANSIOS */|#
  ( #x0ef9 #x11f0 ) #|/*  Hangul_J_KkogjiDalrinIeung ᇰ HANGUL JONGSEONG YESIEUNG */|#
  ( #x0efa #x11f9 ) #|/*        Hangul_J_YeorinHieuh ᇹ HANGUL JONGSEONG YEORINHIEUH */|#
  ( #x0eff #x20a9 ) #|/*                  Korean_Won ₩ WON SIGN */|#
  ( #x13a4 #x20ac ) #|/*                        Euro € EURO SIGN */|#
  ( #x13bc #x0152 ) #|/*                          OE Œ LATIN CAPITAL LIGATURE OE */|#
  ( #x13bd #x0153 ) #|/*                          oe œ LATIN SMALL LIGATURE OE */|#
  ( #x13be #x0178 ) #|/*                  Ydiaeresis Ÿ LATIN CAPITAL LETTER Y WITH DIAERESIS */|#
  ( #x20a0 #x20a0 ) #|/*                     EcuSign ₠ EURO-CURRENCY SIGN */|#
  ( #x20a1 #x20a1 ) #|/*                   ColonSign ₡ COLON SIGN */|#
  ( #x20a2 #x20a2 ) #|/*                CruzeiroSign ₢ CRUZEIRO SIGN */|#
  ( #x20a3 #x20a3 ) #|/*                  FFrancSign ₣ FRENCH FRANC SIGN */|#
  ( #x20a4 #x20a4 ) #|/*                    LiraSign ₤ LIRA SIGN */|#
  ( #x20a5 #x20a5 ) #|/*                    MillSign ₥ MILL SIGN */|#
  ( #x20a6 #x20a6 ) #|/*                   NairaSign ₦ NAIRA SIGN */|#
  ( #x20a7 #x20a7 ) #|/*                  PesetaSign ₧ PESETA SIGN */|#
  ( #x20a8 #x20a8 ) #|/*                   RupeeSign ₨ RUPEE SIGN */|#
  ( #x20a9 #x20a9 ) #|/*                     WonSign ₩ WON SIGN */|#
  ( #x20aa #x20aa ) #|/*               NewSheqelSign ₪ NEW SHEQEL SIGN */|#
  ( #x20ab #x20ab ) #|/*                    DongSign ₫ DONG SIGN */|#
  ( #x20ac #x20ac ) #|/*                    EuroSign € EURO SIGN */|#

  #|/* Following items added to GTK, not in the xterm table */|#

  #|/* A few ASCII control characters */|#

  ( #xFF08 #|/* Backspace '\b' */|#  #.(char-code #\Backspace))
  ( #xFF09 #|/* Tab       \t' */|# ' #.(char-code  #\Tab ))
  ( #xFF0A #|/* Linefeed  '\n' */|#  #.(char-code #\Newline ))
  ( #xFF0B #|/* Vert. Tab '\v' */|#  #.(char-code #\PageUp ))
  ( #xFF0D #|/* Return    '\r'*/|#  #.(char-code #\Return ))
  ( #xFF1B #|/* Escape    '\033' */|#  #.(char-code #\Escape))

  #|/* Numeric keypad */|#

  ( #xFF80 #|/* Space     ' ' */|# #.(char-code #\  ))
  ( #xFFAA #|/* Multiply  '*' */|# #.(char-code #\* ))
  ( #xFFAB #|/* Add       '+' */|# #.(char-code #\+ ))
  ( #xFFAC #|/* Separator ',' */|# #.(char-code #\, ))
  ( #xFFAD #|/* Subtract  '-' */|# #.(char-code #\- ))
  ( #xFFAE #|/* Decimal   '.' */|# #.(char-code #\. ))
  ( #xFFAF #|/* Divide    '/' */|# #.(char-code #\/ ))
  ( #xFFB0 #|/* 0 */|# #.(char-code #\0 ))
  ( #xFFB1 #|/* 1 */|# #.(char-code #\1 ))
  ( #xFFB2 #|/* 2 */|# #.(char-code #\2 ))
  ( #xFFB3 #|/* 3 */|# #.(char-code #\3 ))
  ( #xFFB4 #|/* 4 */|# #.(char-code #\4 ))
  ( #xFFB5 #|/* 5 */|# #.(char-code #\5 ))
  ( #xFFB6 #|/* 6 */|# #.(char-code #\6 ))
  ( #xFFB7 #|/* 7 */|# #.(char-code #\7 ))
  ( #xFFB8 #|/* 8 */|# #.(char-code #\8 ))
  ( #xFFB9 #|/* 9 */|# #.(char-code #\9 ))
  ( #xFFBD #|/* Equal */|# #.(char-code #\= ))

  #|/* End numeric keypad */|#

  ( #xFFFF #|/* Delete '\177' */|# #o177  )
))

(defun gdk-keyval-to-unicode (keyval)
  "/**
 * gdk_keyval_to_unicode:
 * @keyval: a GDK key symbol
 *
 * Convert from a GDK key symbol to the corresponding Unicode
 * character.
 *
 * Note that the conversion does not take the current locale
 * into consideration, which might be expected for particular
 * keyvals, such as %GDK_KEY_KP_Decimal.
 *
 * Returns: the corresponding unicode character, or 0 if there
 *   is no corresponding character.
 */"
  (or
   #|/* First check for Latin-1 characters (1:1 mapping) */|#
   (if (or (and (>= keyval #x0020) (<= keyval #x007e))
	   (and (>= keyval #x00a0) (<= keyval #x00ff)))
       keyval)

   #|/* Also check for directly encoded 24-bit UCS characters: */|#
   (if (= (logand keyval #xff000000)  #x01000000)
       (logand keyval  #x00ffffff))

   #|/* binary search in table */|#
   (loop with min = 0
	 with max = (1- (length *gdk-keysym-to-unicode-tab*))
	 while (>= max min)
	 do (let* ((mid (truncate (+ min max) 2))
		   (elt (elt *gdk-keysym-to-unicode-tab* mid)))
	      (destructuring-bind (keysym ucs) elt
		(cond ((< keysym keyval) (setq min (1+ mid)))
		      ((> keysym keyval) (setq max (1- mid)))
		      (t #|/* found it */|#
		       (return ucs))))))
   #|/* No matching Unicode value found */|#
   0))

(type-of (gdk-keyval-to-unicode #xff08))

#||
static const struct (
  unsigned short keysym;
  unsigned short ucs;
} gdk_unicode_to_keysym_tab[]
|#

(defvar *gdk-unicode-to-keysym-tab*
 '(
  ( #x0abd #x002e ) #|/*                decimalpoint . FULL STOP */|#
  ( #x0ba3 #x003c ) #|/*                   leftcaret < LESS-THAN SIGN */|#
  ( #x0ba6 #x003e ) #|/*                  rightcaret > GREATER-THAN SIGN */|#
  ( #x0bc6 #x005f ) #|/*                    underbar _ LOW LINE */|#
  ( #x0bc0 #x00af ) #|/*                     overbar ¯ MACRON */|#
  ( #x03c0 #x0100 ) #|/*                     Amacron Ā LATIN CAPITAL LETTER A WITH MACRON */|#
  ( #x03e0 #x0101 ) #|/*                     amacron ā LATIN SMALL LETTER A WITH MACRON */|#
  ( #x01c3 #x0102 ) #|/*                      Abreve Ă LATIN CAPITAL LETTER A WITH BREVE */|#
  ( #x01e3 #x0103 ) #|/*                      abreve ă LATIN SMALL LETTER A WITH BREVE */|#
  ( #x01a1 #x0104 ) #|/*                     Aogonek Ą LATIN CAPITAL LETTER A WITH OGONEK */|#
  ( #x01b1 #x0105 ) #|/*                     aogonek ą LATIN SMALL LETTER A WITH OGONEK */|#
  ( #x01c6 #x0106 ) #|/*                      Cacute Ć LATIN CAPITAL LETTER C WITH ACUTE */|#
  ( #x01e6 #x0107 ) #|/*                      cacute ć LATIN SMALL LETTER C WITH ACUTE */|#
  ( #x02c6 #x0108 ) #|/*                 Ccircumflex Ĉ LATIN CAPITAL LETTER C WITH CIRCUMFLEX */|#
  ( #x02e6 #x0109 ) #|/*                 ccircumflex ĉ LATIN SMALL LETTER C WITH CIRCUMFLEX */|#
  ( #x02c5 #x010a ) #|/*                   Cabovedot Ċ LATIN CAPITAL LETTER C WITH DOT ABOVE */|#
  ( #x02e5 #x010b ) #|/*                   cabovedot ċ LATIN SMALL LETTER C WITH DOT ABOVE */|#
  ( #x01c8 #x010c ) #|/*                      Ccaron Č LATIN CAPITAL LETTER C WITH CARON */|#
  ( #x01e8 #x010d ) #|/*                      ccaron č LATIN SMALL LETTER C WITH CARON */|#
  ( #x01cf #x010e ) #|/*                      Dcaron Ď LATIN CAPITAL LETTER D WITH CARON */|#
  ( #x01ef #x010f ) #|/*                      dcaron ď LATIN SMALL LETTER D WITH CARON */|#
  ( #x01d0 #x0110 ) #|/*                     Dstroke Đ LATIN CAPITAL LETTER D WITH STROKE */|#
  ( #x01f0 #x0111 ) #|/*                     dstroke đ LATIN SMALL LETTER D WITH STROKE */|#
  ( #x03aa #x0112 ) #|/*                     Emacron Ē LATIN CAPITAL LETTER E WITH MACRON */|#
  ( #x03ba #x0113 ) #|/*                     emacron ē LATIN SMALL LETTER E WITH MACRON */|#
  ( #x03cc #x0116 ) #|/*                   Eabovedot Ė LATIN CAPITAL LETTER E WITH DOT ABOVE */|#
  ( #x03ec #x0117 ) #|/*                   eabovedot ė LATIN SMALL LETTER E WITH DOT ABOVE */|#
  ( #x01ca #x0118 ) #|/*                     Eogonek Ę LATIN CAPITAL LETTER E WITH OGONEK */|#
  ( #x01ea #x0119 ) #|/*                     eogonek ę LATIN SMALL LETTER E WITH OGONEK */|#
  ( #x01cc #x011a ) #|/*                      Ecaron Ě LATIN CAPITAL LETTER E WITH CARON */|#
  ( #x01ec #x011b ) #|/*                      ecaron ě LATIN SMALL LETTER E WITH CARON */|#
  ( #x02d8 #x011c ) #|/*                 Gcircumflex Ĝ LATIN CAPITAL LETTER G WITH CIRCUMFLEX */|#
  ( #x02f8 #x011d ) #|/*                 gcircumflex ĝ LATIN SMALL LETTER G WITH CIRCUMFLEX */|#
  ( #x02ab #x011e ) #|/*                      Gbreve Ğ LATIN CAPITAL LETTER G WITH BREVE */|#
  ( #x02bb #x011f ) #|/*                      gbreve ğ LATIN SMALL LETTER G WITH BREVE */|#
  ( #x02d5 #x0120 ) #|/*                   Gabovedot Ġ LATIN CAPITAL LETTER G WITH DOT ABOVE */|#
  ( #x02f5 #x0121 ) #|/*                   gabovedot ġ LATIN SMALL LETTER G WITH DOT ABOVE */|#
  ( #x03ab #x0122 ) #|/*                    Gcedilla Ģ LATIN CAPITAL LETTER G WITH CEDILLA */|#
  ( #x03bb #x0123 ) #|/*                    gcedilla ģ LATIN SMALL LETTER G WITH CEDILLA */|#
  ( #x02a6 #x0124 ) #|/*                 Hcircumflex Ĥ LATIN CAPITAL LETTER H WITH CIRCUMFLEX */|#
  ( #x02b6 #x0125 ) #|/*                 hcircumflex ĥ LATIN SMALL LETTER H WITH CIRCUMFLEX */|#
  ( #x02a1 #x0126 ) #|/*                     Hstroke Ħ LATIN CAPITAL LETTER H WITH STROKE */|#
  ( #x02b1 #x0127 ) #|/*                     hstroke ħ LATIN SMALL LETTER H WITH STROKE */|#
  ( #x03a5 #x0128 ) #|/*                      Itilde Ĩ LATIN CAPITAL LETTER I WITH TILDE */|#
  ( #x03b5 #x0129 ) #|/*                      itilde ĩ LATIN SMALL LETTER I WITH TILDE */|#
  ( #x03cf #x012a ) #|/*                     Imacron Ī LATIN CAPITAL LETTER I WITH MACRON */|#
  ( #x03ef #x012b ) #|/*                     imacron ī LATIN SMALL LETTER I WITH MACRON */|#
  ( #x03c7 #x012e ) #|/*                     Iogonek Į LATIN CAPITAL LETTER I WITH OGONEK */|#
  ( #x03e7 #x012f ) #|/*                     iogonek į LATIN SMALL LETTER I WITH OGONEK */|#
  ( #x02a9 #x0130 ) #|/*                   Iabovedot İ LATIN CAPITAL LETTER I WITH DOT ABOVE */|#
  ( #x02b9 #x0131 ) #|/*                    idotless ı LATIN SMALL LETTER DOTLESS I */|#
  ( #x02ac #x0134 ) #|/*                 Jcircumflex Ĵ LATIN CAPITAL LETTER J WITH CIRCUMFLEX */|#
  ( #x02bc #x0135 ) #|/*                 jcircumflex ĵ LATIN SMALL LETTER J WITH CIRCUMFLEX */|#
  ( #x03d3 #x0136 ) #|/*                    Kcedilla Ķ LATIN CAPITAL LETTER K WITH CEDILLA */|#
  ( #x03f3 #x0137 ) #|/*                    kcedilla ķ LATIN SMALL LETTER K WITH CEDILLA */|#
  ( #x03a2 #x0138 ) #|/*                         kra ĸ LATIN SMALL LETTER KRA */|#
  ( #x01c5 #x0139 ) #|/*                      Lacute Ĺ LATIN CAPITAL LETTER L WITH ACUTE */|#
  ( #x01e5 #x013a ) #|/*                      lacute ĺ LATIN SMALL LETTER L WITH ACUTE */|#
  ( #x03a6 #x013b ) #|/*                    Lcedilla Ļ LATIN CAPITAL LETTER L WITH CEDILLA */|#
  ( #x03b6 #x013c ) #|/*                    lcedilla ļ LATIN SMALL LETTER L WITH CEDILLA */|#
  ( #x01a5 #x013d ) #|/*                      Lcaron Ľ LATIN CAPITAL LETTER L WITH CARON */|#
  ( #x01b5 #x013e ) #|/*                      lcaron ľ LATIN SMALL LETTER L WITH CARON */|#
  ( #x01a3 #x0141 ) #|/*                     Lstroke Ł LATIN CAPITAL LETTER L WITH STROKE */|#
  ( #x01b3 #x0142 ) #|/*                     lstroke ł LATIN SMALL LETTER L WITH STROKE */|#
  ( #x01d1 #x0143 ) #|/*                      Nacute Ń LATIN CAPITAL LETTER N WITH ACUTE */|#
  ( #x01f1 #x0144 ) #|/*                      nacute ń LATIN SMALL LETTER N WITH ACUTE */|#
  ( #x03d1 #x0145 ) #|/*                    Ncedilla Ņ LATIN CAPITAL LETTER N WITH CEDILLA */|#
  ( #x03f1 #x0146 ) #|/*                    ncedilla ņ LATIN SMALL LETTER N WITH CEDILLA */|#
  ( #x01d2 #x0147 ) #|/*                      Ncaron Ň LATIN CAPITAL LETTER N WITH CARON */|#
  ( #x01f2 #x0148 ) #|/*                      ncaron ň LATIN SMALL LETTER N WITH CARON */|#
  ( #x03bd #x014a ) #|/*                         ENG Ŋ LATIN CAPITAL LETTER ENG */|#
  ( #x03bf #x014b ) #|/*                         eng ŋ LATIN SMALL LETTER ENG */|#
  ( #x03d2 #x014c ) #|/*                     Omacron Ō LATIN CAPITAL LETTER O WITH MACRON */|#
  ( #x03f2 #x014d ) #|/*                     omacron ō LATIN SMALL LETTER O WITH MACRON */|#
  ( #x01d5 #x0150 ) #|/*                Odoubleacute Ő LATIN CAPITAL LETTER O WITH DOUBLE ACUTE */|#
  ( #x01f5 #x0151 ) #|/*                odoubleacute ő LATIN SMALL LETTER O WITH DOUBLE ACUTE */|#
  ( #x13bc #x0152 ) #|/*                          OE Œ LATIN CAPITAL LIGATURE OE */|#
  ( #x13bd #x0153 ) #|/*                          oe œ LATIN SMALL LIGATURE OE */|#
  ( #x01c0 #x0154 ) #|/*                      Racute Ŕ LATIN CAPITAL LETTER R WITH ACUTE */|#
  ( #x01e0 #x0155 ) #|/*                      racute ŕ LATIN SMALL LETTER R WITH ACUTE */|#
  ( #x03a3 #x0156 ) #|/*                    Rcedilla Ŗ LATIN CAPITAL LETTER R WITH CEDILLA */|#
  ( #x03b3 #x0157 ) #|/*                    rcedilla ŗ LATIN SMALL LETTER R WITH CEDILLA */|#
  ( #x01d8 #x0158 ) #|/*                      Rcaron Ř LATIN CAPITAL LETTER R WITH CARON */|#
  ( #x01f8 #x0159 ) #|/*                      rcaron ř LATIN SMALL LETTER R WITH CARON */|#
  ( #x01a6 #x015a ) #|/*                      Sacute Ś LATIN CAPITAL LETTER S WITH ACUTE */|#
  ( #x01b6 #x015b ) #|/*                      sacute ś LATIN SMALL LETTER S WITH ACUTE */|#
  ( #x02de #x015c ) #|/*                 Scircumflex Ŝ LATIN CAPITAL LETTER S WITH CIRCUMFLEX */|#
  ( #x02fe #x015d ) #|/*                 scircumflex ŝ LATIN SMALL LETTER S WITH CIRCUMFLEX */|#
  ( #x01aa #x015e ) #|/*                    Scedilla Ş LATIN CAPITAL LETTER S WITH CEDILLA */|#
  ( #x01ba #x015f ) #|/*                    scedilla ş LATIN SMALL LETTER S WITH CEDILLA */|#
  ( #x01a9 #x0160 ) #|/*                      Scaron Š LATIN CAPITAL LETTER S WITH CARON */|#
  ( #x01b9 #x0161 ) #|/*                      scaron š LATIN SMALL LETTER S WITH CARON */|#
  ( #x01de #x0162 ) #|/*                    Tcedilla Ţ LATIN CAPITAL LETTER T WITH CEDILLA */|#
  ( #x01fe #x0163 ) #|/*                    tcedilla ţ LATIN SMALL LETTER T WITH CEDILLA */|#
  ( #x01ab #x0164 ) #|/*                      Tcaron Ť LATIN CAPITAL LETTER T WITH CARON */|#
  ( #x01bb #x0165 ) #|/*                      tcaron ť LATIN SMALL LETTER T WITH CARON */|#
  ( #x03ac #x0166 ) #|/*                      Tslash Ŧ LATIN CAPITAL LETTER T WITH STROKE */|#
  ( #x03bc #x0167 ) #|/*                      tslash ŧ LATIN SMALL LETTER T WITH STROKE */|#
  ( #x03dd #x0168 ) #|/*                      Utilde Ũ LATIN CAPITAL LETTER U WITH TILDE */|#
  ( #x03fd #x0169 ) #|/*                      utilde ũ LATIN SMALL LETTER U WITH TILDE */|#
  ( #x03de #x016a ) #|/*                     Umacron Ū LATIN CAPITAL LETTER U WITH MACRON */|#
  ( #x03fe #x016b ) #|/*                     umacron ū LATIN SMALL LETTER U WITH MACRON */|#
  ( #x02dd #x016c ) #|/*                      Ubreve Ŭ LATIN CAPITAL LETTER U WITH BREVE */|#
  ( #x02fd #x016d ) #|/*                      ubreve ŭ LATIN SMALL LETTER U WITH BREVE */|#
  ( #x01d9 #x016e ) #|/*                       Uring Ů LATIN CAPITAL LETTER U WITH RING ABOVE */|#
  ( #x01f9 #x016f ) #|/*                       uring ů LATIN SMALL LETTER U WITH RING ABOVE */|#
  ( #x01db #x0170 ) #|/*                Udoubleacute Ű LATIN CAPITAL LETTER U WITH DOUBLE ACUTE */|#
  ( #x01fb #x0171 ) #|/*                udoubleacute ű LATIN SMALL LETTER U WITH DOUBLE ACUTE */|#
  ( #x03d9 #x0172 ) #|/*                     Uogonek Ų LATIN CAPITAL LETTER U WITH OGONEK */|#
  ( #x03f9 #x0173 ) #|/*                     uogonek ų LATIN SMALL LETTER U WITH OGONEK */|#
  ( #x13be #x0178 ) #|/*                  Ydiaeresis Ÿ LATIN CAPITAL LETTER Y WITH DIAERESIS */|#
  ( #x01ac #x0179 ) #|/*                      Zacute Ź LATIN CAPITAL LETTER Z WITH ACUTE */|#
  ( #x01bc #x017a ) #|/*                      zacute ź LATIN SMALL LETTER Z WITH ACUTE */|#
  ( #x01af #x017b ) #|/*                   Zabovedot Ż LATIN CAPITAL LETTER Z WITH DOT ABOVE */|#
  ( #x01bf #x017c ) #|/*                   zabovedot ż LATIN SMALL LETTER Z WITH DOT ABOVE */|#
  ( #x01ae #x017d ) #|/*                      Zcaron Ž LATIN CAPITAL LETTER Z WITH CARON */|#
  ( #x01be #x017e ) #|/*                      zcaron ž LATIN SMALL LETTER Z WITH CARON */|#
  ( #x08f6 #x0192 ) #|/*                    function ƒ LATIN SMALL LETTER F WITH HOOK */|#
  ( #x01b7 #x02c7 ) #|/*                       caron ˇ CARON */|#
  ( #x01a2 #x02d8 ) #|/*                       breve ˘ BREVE */|#
  ( #x01ff #x02d9 ) #|/*                    abovedot ˙ DOT ABOVE */|#
  ( #x01b2 #x02db ) #|/*                      ogonek ˛ OGONEK */|#
  ( #x01bd #x02dd ) #|/*                 doubleacute ˝ DOUBLE ACUTE ACCENT */|#
  ( #x07ae #x0385 ) #|/*        Greek_accentdieresis ΅ GREEK DIALYTIKA TONOS */|#
  ( #x07a1 #x0386 ) #|/*           Greek_ALPHAaccent Ά GREEK CAPITAL LETTER ALPHA WITH TONOS */|#
  ( #x07a2 #x0388 ) #|/*         Greek_EPSILONaccent Έ GREEK CAPITAL LETTER EPSILON WITH TONOS */|#
  ( #x07a3 #x0389 ) #|/*             Greek_ETAaccent Ή GREEK CAPITAL LETTER ETA WITH TONOS */|#
  ( #x07a4 #x038a ) #|/*            Greek_IOTAaccent Ί GREEK CAPITAL LETTER IOTA WITH TONOS */|#
  ( #x07a7 #x038c ) #|/*         Greek_OMICRONaccent Ό GREEK CAPITAL LETTER OMICRON WITH TONOS */|#
  ( #x07a8 #x038e ) #|/*         Greek_UPSILONaccent Ύ GREEK CAPITAL LETTER UPSILON WITH TONOS */|#
  ( #x07ab #x038f ) #|/*           Greek_OMEGAaccent Ώ GREEK CAPITAL LETTER OMEGA WITH TONOS */|#
  ( #x07b6 #x0390 ) #|/*    Greek_iotaaccentdieresis ΐ GREEK SMALL LETTER IOTA WITH DIALYTIKA AND TONOS */|#
  ( #x07c1 #x0391 ) #|/*                 Greek_ALPHA Α GREEK CAPITAL LETTER ALPHA */|#
  ( #x07c2 #x0392 ) #|/*                  Greek_BETA Β GREEK CAPITAL LETTER BETA */|#
  ( #x07c3 #x0393 ) #|/*                 Greek_GAMMA Γ GREEK CAPITAL LETTER GAMMA */|#
  ( #x07c4 #x0394 ) #|/*                 Greek_DELTA Δ GREEK CAPITAL LETTER DELTA */|#
  ( #x07c5 #x0395 ) #|/*               Greek_EPSILON Ε GREEK CAPITAL LETTER EPSILON */|#
  ( #x07c6 #x0396 ) #|/*                  Greek_ZETA Ζ GREEK CAPITAL LETTER ZETA */|#
  ( #x07c7 #x0397 ) #|/*                   Greek_ETA Η GREEK CAPITAL LETTER ETA */|#
  ( #x07c8 #x0398 ) #|/*                 Greek_THETA Θ GREEK CAPITAL LETTER THETA */|#
  ( #x07c9 #x0399 ) #|/*                  Greek_IOTA Ι GREEK CAPITAL LETTER IOTA */|#
  ( #x07ca #x039a ) #|/*                 Greek_KAPPA Κ GREEK CAPITAL LETTER KAPPA */|#
  ( #x07cb #x039b ) #|/*                Greek_LAMBDA Λ GREEK CAPITAL LETTER LAMDA */|#
  ( #x07cc #x039c ) #|/*                    Greek_MU Μ GREEK CAPITAL LETTER MU */|#
  ( #x07cd #x039d ) #|/*                    Greek_NU Ν GREEK CAPITAL LETTER NU */|#
  ( #x07ce #x039e ) #|/*                    Greek_XI Ξ GREEK CAPITAL LETTER XI */|#
  ( #x07cf #x039f ) #|/*               Greek_OMICRON Ο GREEK CAPITAL LETTER OMICRON */|#
  ( #x07d0 #x03a0 ) #|/*                    Greek_PI Π GREEK CAPITAL LETTER PI */|#
  ( #x07d1 #x03a1 ) #|/*                   Greek_RHO Ρ GREEK CAPITAL LETTER RHO */|#
  ( #x07d2 #x03a3 ) #|/*                 Greek_SIGMA Σ GREEK CAPITAL LETTER SIGMA */|#
  ( #x07d4 #x03a4 ) #|/*                   Greek_TAU Τ GREEK CAPITAL LETTER TAU */|#
  ( #x07d5 #x03a5 ) #|/*               Greek_UPSILON Υ GREEK CAPITAL LETTER UPSILON */|#
  ( #x07d6 #x03a6 ) #|/*                   Greek_PHI Φ GREEK CAPITAL LETTER PHI */|#
  ( #x07d7 #x03a7 ) #|/*                   Greek_CHI Χ GREEK CAPITAL LETTER CHI */|#
  ( #x07d8 #x03a8 ) #|/*                   Greek_PSI Ψ GREEK CAPITAL LETTER PSI */|#
  ( #x07d9 #x03a9 ) #|/*                 Greek_OMEGA Ω GREEK CAPITAL LETTER OMEGA */|#
  ( #x07a5 #x03aa ) #|/*         Greek_IOTAdiaeresis Ϊ GREEK CAPITAL LETTER IOTA WITH DIALYTIKA */|#
  ( #x07a9 #x03ab ) #|/*       Greek_UPSILONdieresis Ϋ GREEK CAPITAL LETTER UPSILON WITH DIALYTIKA */|#
  ( #x07b1 #x03ac ) #|/*           Greek_alphaaccent ά GREEK SMALL LETTER ALPHA WITH TONOS */|#
  ( #x07b2 #x03ad ) #|/*         Greek_epsilonaccent έ GREEK SMALL LETTER EPSILON WITH TONOS */|#
  ( #x07b3 #x03ae ) #|/*             Greek_etaaccent ή GREEK SMALL LETTER ETA WITH TONOS */|#
  ( #x07b4 #x03af ) #|/*            Greek_iotaaccent ί GREEK SMALL LETTER IOTA WITH TONOS */|#
  ( #x07ba #x03b0 ) #|/* Greek_upsilonaccentdieresis ΰ GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND TONOS */|#
  ( #x07e1 #x03b1 ) #|/*                 Greek_alpha α GREEK SMALL LETTER ALPHA */|#
  ( #x07e2 #x03b2 ) #|/*                  Greek_beta β GREEK SMALL LETTER BETA */|#
  ( #x07e3 #x03b3 ) #|/*                 Greek_gamma γ GREEK SMALL LETTER GAMMA */|#
  ( #x07e4 #x03b4 ) #|/*                 Greek_delta δ GREEK SMALL LETTER DELTA */|#
  ( #x07e5 #x03b5 ) #|/*               Greek_epsilon ε GREEK SMALL LETTER EPSILON */|#
  ( #x07e6 #x03b6 ) #|/*                  Greek_zeta ζ GREEK SMALL LETTER ZETA */|#
  ( #x07e7 #x03b7 ) #|/*                   Greek_eta η GREEK SMALL LETTER ETA */|#
  ( #x07e8 #x03b8 ) #|/*                 Greek_theta θ GREEK SMALL LETTER THETA */|#
  ( #x07e9 #x03b9 ) #|/*                  Greek_iota ι GREEK SMALL LETTER IOTA */|#
  ( #x07ea #x03ba ) #|/*                 Greek_kappa κ GREEK SMALL LETTER KAPPA */|#
  ( #x07eb #x03bb ) #|/*                Greek_lambda λ GREEK SMALL LETTER LAMDA */|#
  ( #x07ec #x03bc ) #|/*                    Greek_mu μ GREEK SMALL LETTER MU */|#
  ( #x07ed #x03bd ) #|/*                    Greek_nu ν GREEK SMALL LETTER NU */|#
  ( #x07ee #x03be ) #|/*                    Greek_xi ξ GREEK SMALL LETTER XI */|#
  ( #x07ef #x03bf ) #|/*               Greek_omicron ο GREEK SMALL LETTER OMICRON */|#
  ( #x07f0 #x03c0 ) #|/*                    Greek_pi π GREEK SMALL LETTER PI */|#
  ( #x07f1 #x03c1 ) #|/*                   Greek_rho ρ GREEK SMALL LETTER RHO */|#
  ( #x07f3 #x03c2 ) #|/*       Greek_finalsmallsigma ς GREEK SMALL LETTER FINAL SIGMA */|#
  ( #x07f2 #x03c3 ) #|/*                 Greek_sigma σ GREEK SMALL LETTER SIGMA */|#
  ( #x07f4 #x03c4 ) #|/*                   Greek_tau τ GREEK SMALL LETTER TAU */|#
  ( #x07f5 #x03c5 ) #|/*               Greek_upsilon υ GREEK SMALL LETTER UPSILON */|#
  ( #x07f6 #x03c6 ) #|/*                   Greek_phi φ GREEK SMALL LETTER PHI */|#
  ( #x07f7 #x03c7 ) #|/*                   Greek_chi χ GREEK SMALL LETTER CHI */|#
  ( #x07f8 #x03c8 ) #|/*                   Greek_psi ψ GREEK SMALL LETTER PSI */|#
  ( #x07f9 #x03c9 ) #|/*                 Greek_omega ω GREEK SMALL LETTER OMEGA */|#
  ( #x07b5 #x03ca ) #|/*          Greek_iotadieresis ϊ GREEK SMALL LETTER IOTA WITH DIALYTIKA */|#
  ( #x07b9 #x03cb ) #|/*       Greek_upsilondieresis ϋ GREEK SMALL LETTER UPSILON WITH DIALYTIKA */|#
  ( #x07b7 #x03cc ) #|/*         Greek_omicronaccent ό GREEK SMALL LETTER OMICRON WITH TONOS */|#
  ( #x07b8 #x03cd ) #|/*         Greek_upsilonaccent ύ GREEK SMALL LETTER UPSILON WITH TONOS */|#
  ( #x07bb #x03ce ) #|/*           Greek_omegaaccent ώ GREEK SMALL LETTER OMEGA WITH TONOS */|#
  ( #x06b3 #x0401 ) #|/*                 Cyrillic_IO Ё CYRILLIC CAPITAL LETTER IO */|#
  ( #x06b1 #x0402 ) #|/*                 Serbian_DJE Ђ CYRILLIC CAPITAL LETTER DJE */|#
  ( #x06b2 #x0403 ) #|/*               Macedonia_GJE Ѓ CYRILLIC CAPITAL LETTER GJE */|#
  ( #x06b4 #x0404 ) #|/*                Ukrainian_IE Є CYRILLIC CAPITAL LETTER UKRAINIAN IE */|#
  ( #x06b5 #x0405 ) #|/*               Macedonia_DSE Ѕ CYRILLIC CAPITAL LETTER DZE */|#
  ( #x06b6 #x0406 ) #|/*                 Ukrainian_I І CYRILLIC CAPITAL LETTER BYELORUSSIAN-UKRAINIAN I */|#
  ( #x06b7 #x0407 ) #|/*                Ukrainian_YI Ї CYRILLIC CAPITAL LETTER YI */|#
  ( #x06b8 #x0408 ) #|/*                 Cyrillic_JE Ј CYRILLIC CAPITAL LETTER JE */|#
  ( #x06b9 #x0409 ) #|/*                Cyrillic_LJE Љ CYRILLIC CAPITAL LETTER LJE */|#
  ( #x06ba #x040a ) #|/*                Cyrillic_NJE Њ CYRILLIC CAPITAL LETTER NJE */|#
  ( #x06bb #x040b ) #|/*                Serbian_TSHE Ћ CYRILLIC CAPITAL LETTER TSHE */|#
  ( #x06bc #x040c ) #|/*               Macedonia_KJE Ќ CYRILLIC CAPITAL LETTER KJE */|#
  ( #x06be #x040e ) #|/*         Byelorussian_SHORTU Ў CYRILLIC CAPITAL LETTER SHORT U */|#
  ( #x06bf #x040f ) #|/*               Cyrillic_DZHE Џ CYRILLIC CAPITAL LETTER DZHE */|#
  ( #x06e1 #x0410 ) #|/*                  Cyrillic_A А CYRILLIC CAPITAL LETTER A */|#
  ( #x06e2 #x0411 ) #|/*                 Cyrillic_BE Б CYRILLIC CAPITAL LETTER BE */|#
  ( #x06f7 #x0412 ) #|/*                 Cyrillic_VE В CYRILLIC CAPITAL LETTER VE */|#
  ( #x06e7 #x0413 ) #|/*                Cyrillic_GHE Г CYRILLIC CAPITAL LETTER GHE */|#
  ( #x06e4 #x0414 ) #|/*                 Cyrillic_DE Д CYRILLIC CAPITAL LETTER DE */|#
  ( #x06e5 #x0415 ) #|/*                 Cyrillic_IE Е CYRILLIC CAPITAL LETTER IE */|#
  ( #x06f6 #x0416 ) #|/*                Cyrillic_ZHE Ж CYRILLIC CAPITAL LETTER ZHE */|#
  ( #x06fa #x0417 ) #|/*                 Cyrillic_ZE З CYRILLIC CAPITAL LETTER ZE */|#
  ( #x06e9 #x0418 ) #|/*                  Cyrillic_I И CYRILLIC CAPITAL LETTER I */|#
  ( #x06ea #x0419 ) #|/*             Cyrillic_SHORTI Й CYRILLIC CAPITAL LETTER SHORT I */|#
  ( #x06eb #x041a ) #|/*                 Cyrillic_KA К CYRILLIC CAPITAL LETTER KA */|#
  ( #x06ec #x041b ) #|/*                 Cyrillic_EL Л CYRILLIC CAPITAL LETTER EL */|#
  ( #x06ed #x041c ) #|/*                 Cyrillic_EM М CYRILLIC CAPITAL LETTER EM */|#
  ( #x06ee #x041d ) #|/*                 Cyrillic_EN Н CYRILLIC CAPITAL LETTER EN */|#
  ( #x06ef #x041e ) #|/*                  Cyrillic_O О CYRILLIC CAPITAL LETTER O */|#
  ( #x06f0 #x041f ) #|/*                 Cyrillic_PE П CYRILLIC CAPITAL LETTER PE */|#
  ( #x06f2 #x0420 ) #|/*                 Cyrillic_ER Р CYRILLIC CAPITAL LETTER ER */|#
  ( #x06f3 #x0421 ) #|/*                 Cyrillic_ES С CYRILLIC CAPITAL LETTER ES */|#
  ( #x06f4 #x0422 ) #|/*                 Cyrillic_TE Т CYRILLIC CAPITAL LETTER TE */|#
  ( #x06f5 #x0423 ) #|/*                  Cyrillic_U У CYRILLIC CAPITAL LETTER U */|#
  ( #x06e6 #x0424 ) #|/*                 Cyrillic_EF Ф CYRILLIC CAPITAL LETTER EF */|#
  ( #x06e8 #x0425 ) #|/*                 Cyrillic_HA Х CYRILLIC CAPITAL LETTER HA */|#
  ( #x06e3 #x0426 ) #|/*                Cyrillic_TSE Ц CYRILLIC CAPITAL LETTER TSE */|#
  ( #x06fe #x0427 ) #|/*                Cyrillic_CHE Ч CYRILLIC CAPITAL LETTER CHE */|#
  ( #x06fb #x0428 ) #|/*                Cyrillic_SHA Ш CYRILLIC CAPITAL LETTER SHA */|#
  ( #x06fd #x0429 ) #|/*              Cyrillic_SHCHA Щ CYRILLIC CAPITAL LETTER SHCHA */|#
  ( #x06ff #x042a ) #|/*           Cyrillic_HARDSIGN Ъ CYRILLIC CAPITAL LETTER HARD SIGN */|#
  ( #x06f9 #x042b ) #|/*               Cyrillic_YERU Ы CYRILLIC CAPITAL LETTER YERU */|#
  ( #x06f8 #x042c ) #|/*           Cyrillic_SOFTSIGN Ь CYRILLIC CAPITAL LETTER SOFT SIGN */|#
  ( #x06fc #x042d ) #|/*                  Cyrillic_E Э CYRILLIC CAPITAL LETTER E */|#
  ( #x06e0 #x042e ) #|/*                 Cyrillic_YU Ю CYRILLIC CAPITAL LETTER YU */|#
  ( #x06f1 #x042f ) #|/*                 Cyrillic_YA Я CYRILLIC CAPITAL LETTER YA */|#
  ( #x06c1 #x0430 ) #|/*                  Cyrillic_a а CYRILLIC SMALL LETTER A */|#
  ( #x06c2 #x0431 ) #|/*                 Cyrillic_be б CYRILLIC SMALL LETTER BE */|#
  ( #x06d7 #x0432 ) #|/*                 Cyrillic_ve в CYRILLIC SMALL LETTER VE */|#
  ( #x06c7 #x0433 ) #|/*                Cyrillic_ghe г CYRILLIC SMALL LETTER GHE */|#
  ( #x06c4 #x0434 ) #|/*                 Cyrillic_de д CYRILLIC SMALL LETTER DE */|#
  ( #x06c5 #x0435 ) #|/*                 Cyrillic_ie е CYRILLIC SMALL LETTER IE */|#
  ( #x06d6 #x0436 ) #|/*                Cyrillic_zhe ж CYRILLIC SMALL LETTER ZHE */|#
  ( #x06da #x0437 ) #|/*                 Cyrillic_ze з CYRILLIC SMALL LETTER ZE */|#
  ( #x06c9 #x0438 ) #|/*                  Cyrillic_i и CYRILLIC SMALL LETTER I */|#
  ( #x06ca #x0439 ) #|/*             Cyrillic_shorti й CYRILLIC SMALL LETTER SHORT I */|#
  ( #x06cb #x043a ) #|/*                 Cyrillic_ka к CYRILLIC SMALL LETTER KA */|#
  ( #x06cc #x043b ) #|/*                 Cyrillic_el л CYRILLIC SMALL LETTER EL */|#
  ( #x06cd #x043c ) #|/*                 Cyrillic_em м CYRILLIC SMALL LETTER EM */|#
  ( #x06ce #x043d ) #|/*                 Cyrillic_en н CYRILLIC SMALL LETTER EN */|#
  ( #x06cf #x043e ) #|/*                  Cyrillic_o о CYRILLIC SMALL LETTER O */|#
  ( #x06d0 #x043f ) #|/*                 Cyrillic_pe п CYRILLIC SMALL LETTER PE */|#
  ( #x06d2 #x0440 ) #|/*                 Cyrillic_er р CYRILLIC SMALL LETTER ER */|#
  ( #x06d3 #x0441 ) #|/*                 Cyrillic_es с CYRILLIC SMALL LETTER ES */|#
  ( #x06d4 #x0442 ) #|/*                 Cyrillic_te т CYRILLIC SMALL LETTER TE */|#
  ( #x06d5 #x0443 ) #|/*                  Cyrillic_u у CYRILLIC SMALL LETTER U */|#
  ( #x06c6 #x0444 ) #|/*                 Cyrillic_ef ф CYRILLIC SMALL LETTER EF */|#
  ( #x06c8 #x0445 ) #|/*                 Cyrillic_ha х CYRILLIC SMALL LETTER HA */|#
  ( #x06c3 #x0446 ) #|/*                Cyrillic_tse ц CYRILLIC SMALL LETTER TSE */|#
  ( #x06de #x0447 ) #|/*                Cyrillic_che ч CYRILLIC SMALL LETTER CHE */|#
  ( #x06db #x0448 ) #|/*                Cyrillic_sha ш CYRILLIC SMALL LETTER SHA */|#
  ( #x06dd #x0449 ) #|/*              Cyrillic_shcha щ CYRILLIC SMALL LETTER SHCHA */|#
  ( #x06df #x044a ) #|/*           Cyrillic_hardsign ъ CYRILLIC SMALL LETTER HARD SIGN */|#
  ( #x06d9 #x044b ) #|/*               Cyrillic_yeru ы CYRILLIC SMALL LETTER YERU */|#
  ( #x06d8 #x044c ) #|/*           Cyrillic_softsign ь CYRILLIC SMALL LETTER SOFT SIGN */|#
  ( #x06dc #x044d ) #|/*                  Cyrillic_e э CYRILLIC SMALL LETTER E */|#
  ( #x06c0 #x044e ) #|/*                 Cyrillic_yu ю CYRILLIC SMALL LETTER YU */|#
  ( #x06d1 #x044f ) #|/*                 Cyrillic_ya я CYRILLIC SMALL LETTER YA */|#
  ( #x06a3 #x0451 ) #|/*                 Cyrillic_io ё CYRILLIC SMALL LETTER IO */|#
  ( #x06a1 #x0452 ) #|/*                 Serbian_dje ђ CYRILLIC SMALL LETTER DJE */|#
  ( #x06a2 #x0453 ) #|/*               Macedonia_gje ѓ CYRILLIC SMALL LETTER GJE */|#
  ( #x06a4 #x0454 ) #|/*                Ukrainian_ie є CYRILLIC SMALL LETTER UKRAINIAN IE */|#
  ( #x06a5 #x0455 ) #|/*               Macedonia_dse ѕ CYRILLIC SMALL LETTER DZE */|#
  ( #x06a6 #x0456 ) #|/*                 Ukrainian_i і CYRILLIC SMALL LETTER BYELORUSSIAN-UKRAINIAN I */|#
  ( #x06a7 #x0457 ) #|/*                Ukrainian_yi ї CYRILLIC SMALL LETTER YI */|#
  ( #x06a8 #x0458 ) #|/*                 Cyrillic_je ј CYRILLIC SMALL LETTER JE */|#
  ( #x06a9 #x0459 ) #|/*                Cyrillic_lje љ CYRILLIC SMALL LETTER LJE */|#
  ( #x06aa #x045a ) #|/*                Cyrillic_nje њ CYRILLIC SMALL LETTER NJE */|#
  ( #x06ab #x045b ) #|/*                Serbian_tshe ћ CYRILLIC SMALL LETTER TSHE */|#
  ( #x06ac #x045c ) #|/*               Macedonia_kje ќ CYRILLIC SMALL LETTER KJE */|#
  ( #x06ae #x045e ) #|/*         Byelorussian_shortu ў CYRILLIC SMALL LETTER SHORT U */|#
  ( #x06af #x045f ) #|/*               Cyrillic_dzhe џ CYRILLIC SMALL LETTER DZHE */|#
  ( #x0ce0 #x05d0 ) #|/*                hebrew_aleph א HEBREW LETTER ALEF */|#
  ( #x0ce1 #x05d1 ) #|/*                  hebrew_bet ב HEBREW LETTER BET */|#
  ( #x0ce2 #x05d2 ) #|/*                hebrew_gimel ג HEBREW LETTER GIMEL */|#
  ( #x0ce3 #x05d3 ) #|/*                hebrew_dalet ד HEBREW LETTER DALET */|#
  ( #x0ce4 #x05d4 ) #|/*                   hebrew_he ה HEBREW LETTER HE */|#
  ( #x0ce5 #x05d5 ) #|/*                  hebrew_waw ו HEBREW LETTER VAV */|#
  ( #x0ce6 #x05d6 ) #|/*                 hebrew_zain ז HEBREW LETTER ZAYIN */|#
  ( #x0ce7 #x05d7 ) #|/*                 hebrew_chet ח HEBREW LETTER HET */|#
  ( #x0ce8 #x05d8 ) #|/*                  hebrew_tet ט HEBREW LETTER TET */|#
  ( #x0ce9 #x05d9 ) #|/*                  hebrew_yod י HEBREW LETTER YOD */|#
  ( #x0cea #x05da ) #|/*            hebrew_finalkaph ך HEBREW LETTER FINAL KAF */|#
  ( #x0ceb #x05db ) #|/*                 hebrew_kaph כ HEBREW LETTER KAF */|#
  ( #x0cec #x05dc ) #|/*                hebrew_lamed ל HEBREW LETTER LAMED */|#
  ( #x0ced #x05dd ) #|/*             hebrew_finalmem ם HEBREW LETTER FINAL MEM */|#
  ( #x0cee #x05de ) #|/*                  hebrew_mem מ HEBREW LETTER MEM */|#
  ( #x0cef #x05df ) #|/*             hebrew_finalnun ן HEBREW LETTER FINAL NUN */|#
  ( #x0cf0 #x05e0 ) #|/*                  hebrew_nun נ HEBREW LETTER NUN */|#
  ( #x0cf1 #x05e1 ) #|/*               hebrew_samech ס HEBREW LETTER SAMEKH */|#
  ( #x0cf2 #x05e2 ) #|/*                 hebrew_ayin ע HEBREW LETTER AYIN */|#
  ( #x0cf3 #x05e3 ) #|/*              hebrew_finalpe ף HEBREW LETTER FINAL PE */|#
  ( #x0cf4 #x05e4 ) #|/*                   hebrew_pe פ HEBREW LETTER PE */|#
  ( #x0cf5 #x05e5 ) #|/*            hebrew_finalzade ץ HEBREW LETTER FINAL TSADI */|#
  ( #x0cf6 #x05e6 ) #|/*                 hebrew_zade צ HEBREW LETTER TSADI */|#
  ( #x0cf7 #x05e7 ) #|/*                 hebrew_qoph ק HEBREW LETTER QOF */|#
  ( #x0cf8 #x05e8 ) #|/*                 hebrew_resh ר HEBREW LETTER RESH */|#
  ( #x0cf9 #x05e9 ) #|/*                 hebrew_shin ש HEBREW LETTER SHIN */|#
  ( #x0cfa #x05ea ) #|/*                  hebrew_taw ת HEBREW LETTER TAV */|#
  ( #x05ac #x060c ) #|/*                Arabic_comma ، ARABIC COMMA */|#
  ( #x05bb #x061b ) #|/*            Arabic_semicolon ؛ ARABIC SEMICOLON */|#
  ( #x05bf #x061f ) #|/*        Arabic_question_mark ؟ ARABIC QUESTION MARK */|#
  ( #x05c1 #x0621 ) #|/*                Arabic_hamza ء ARABIC LETTER HAMZA */|#
  ( #x05c2 #x0622 ) #|/*          Arabic_maddaonalef آ ARABIC LETTER ALEF WITH MADDA ABOVE */|#
  ( #x05c3 #x0623 ) #|/*          Arabic_hamzaonalef أ ARABIC LETTER ALEF WITH HAMZA ABOVE */|#
  ( #x05c4 #x0624 ) #|/*           Arabic_hamzaonwaw ؤ ARABIC LETTER WAW WITH HAMZA ABOVE */|#
  ( #x05c5 #x0625 ) #|/*       Arabic_hamzaunderalef إ ARABIC LETTER ALEF WITH HAMZA BELOW */|#
  ( #x05c6 #x0626 ) #|/*           Arabic_hamzaonyeh ئ ARABIC LETTER YEH WITH HAMZA ABOVE */|#
  ( #x05c7 #x0627 ) #|/*                 Arabic_alef ا ARABIC LETTER ALEF */|#
  ( #x05c8 #x0628 ) #|/*                  Arabic_beh ب ARABIC LETTER BEH */|#
  ( #x05c9 #x0629 ) #|/*           Arabic_tehmarbuta ة ARABIC LETTER TEH MARBUTA */|#
  ( #x05ca #x062a ) #|/*                  Arabic_teh ت ARABIC LETTER TEH */|#
  ( #x05cb #x062b ) #|/*                 Arabic_theh ث ARABIC LETTER THEH */|#
  ( #x05cc #x062c ) #|/*                 Arabic_jeem ج ARABIC LETTER JEEM */|#
  ( #x05cd #x062d ) #|/*                  Arabic_hah ح ARABIC LETTER HAH */|#
  ( #x05ce #x062e ) #|/*                 Arabic_khah خ ARABIC LETTER KHAH */|#
  ( #x05cf #x062f ) #|/*                  Arabic_dal د ARABIC LETTER DAL */|#
  ( #x05d0 #x0630 ) #|/*                 Arabic_thal ذ ARABIC LETTER THAL */|#
  ( #x05d1 #x0631 ) #|/*                   Arabic_ra ر ARABIC LETTER REH */|#
  ( #x05d2 #x0632 ) #|/*                 Arabic_zain ز ARABIC LETTER ZAIN */|#
  ( #x05d3 #x0633 ) #|/*                 Arabic_seen س ARABIC LETTER SEEN */|#
  ( #x05d4 #x0634 ) #|/*                Arabic_sheen ش ARABIC LETTER SHEEN */|#
  ( #x05d5 #x0635 ) #|/*                  Arabic_sad ص ARABIC LETTER SAD */|#
  ( #x05d6 #x0636 ) #|/*                  Arabic_dad ض ARABIC LETTER DAD */|#
  ( #x05d7 #x0637 ) #|/*                  Arabic_tah ط ARABIC LETTER TAH */|#
  ( #x05d8 #x0638 ) #|/*                  Arabic_zah ظ ARABIC LETTER ZAH */|#
  ( #x05d9 #x0639 ) #|/*                  Arabic_ain ع ARABIC LETTER AIN */|#
  ( #x05da #x063a ) #|/*                Arabic_ghain غ ARABIC LETTER GHAIN */|#
  ( #x05e0 #x0640 ) #|/*              Arabic_tatweel ـ ARABIC TATWEEL */|#
  ( #x05e1 #x0641 ) #|/*                  Arabic_feh ف ARABIC LETTER FEH */|#
  ( #x05e2 #x0642 ) #|/*                  Arabic_qaf ق ARABIC LETTER QAF */|#
  ( #x05e3 #x0643 ) #|/*                  Arabic_kaf ك ARABIC LETTER KAF */|#
  ( #x05e4 #x0644 ) #|/*                  Arabic_lam ل ARABIC LETTER LAM */|#
  ( #x05e5 #x0645 ) #|/*                 Arabic_meem م ARABIC LETTER MEEM */|#
  ( #x05e6 #x0646 ) #|/*                 Arabic_noon ن ARABIC LETTER NOON */|#
  ( #x05e7 #x0647 ) #|/*                   Arabic_ha ه ARABIC LETTER HEH */|#
  ( #x05e8 #x0648 ) #|/*                  Arabic_waw و ARABIC LETTER WAW */|#
  ( #x05e9 #x0649 ) #|/*          Arabic_alefmaksura ى ARABIC LETTER ALEF MAKSURA */|#
  ( #x05ea #x064a ) #|/*                  Arabic_yeh ي ARABIC LETTER YEH */|#
  ( #x05eb #x064b ) #|/*             Arabic_fathatan ً ARABIC FATHATAN */|#
  ( #x05ec #x064c ) #|/*             Arabic_dammatan ٌ ARABIC DAMMATAN */|#
  ( #x05ed #x064d ) #|/*             Arabic_kasratan ٍ ARABIC KASRATAN */|#
  ( #x05ee #x064e ) #|/*                Arabic_fatha َ ARABIC FATHA */|#
  ( #x05ef #x064f ) #|/*                Arabic_damma ُ ARABIC DAMMA */|#
  ( #x05f0 #x0650 ) #|/*                Arabic_kasra ِ ARABIC KASRA */|#
  ( #x05f1 #x0651 ) #|/*               Arabic_shadda ّ ARABIC SHADDA */|#
  ( #x05f2 #x0652 ) #|/*                Arabic_sukun ْ ARABIC SUKUN */|#
  ( #x0da1 #x0e01 ) #|/*                  Thai_kokai ก THAI CHARACTER KO KAI */|#
  ( #x0da2 #x0e02 ) #|/*                Thai_khokhai ข THAI CHARACTER KHO KHAI */|#
  ( #x0da3 #x0e03 ) #|/*               Thai_khokhuat ฃ THAI CHARACTER KHO KHUAT */|#
  ( #x0da4 #x0e04 ) #|/*               Thai_khokhwai ค THAI CHARACTER KHO KHWAI */|#
  ( #x0da5 #x0e05 ) #|/*                Thai_khokhon ฅ THAI CHARACTER KHO KHON */|#
  ( #x0da6 #x0e06 ) #|/*             Thai_khorakhang ฆ THAI CHARACTER KHO RAKHANG */|#
  ( #x0da7 #x0e07 ) #|/*                 Thai_ngongu ง THAI CHARACTER NGO NGU */|#
  ( #x0da8 #x0e08 ) #|/*                Thai_chochan จ THAI CHARACTER CHO CHAN */|#
  ( #x0da9 #x0e09 ) #|/*               Thai_choching ฉ THAI CHARACTER CHO CHING */|#
  ( #x0daa #x0e0a ) #|/*               Thai_chochang ช THAI CHARACTER CHO CHANG */|#
  ( #x0dab #x0e0b ) #|/*                   Thai_soso ซ THAI CHARACTER SO SO */|#
  ( #x0dac #x0e0c ) #|/*                Thai_chochoe ฌ THAI CHARACTER CHO CHOE */|#
  ( #x0dad #x0e0d ) #|/*                 Thai_yoying ญ THAI CHARACTER YO YING */|#
  ( #x0dae #x0e0e ) #|/*                Thai_dochada ฎ THAI CHARACTER DO CHADA */|#
  ( #x0daf #x0e0f ) #|/*                Thai_topatak ฏ THAI CHARACTER TO PATAK */|#
  ( #x0db0 #x0e10 ) #|/*                Thai_thothan ฐ THAI CHARACTER THO THAN */|#
  ( #x0db1 #x0e11 ) #|/*          Thai_thonangmontho ฑ THAI CHARACTER THO NANGMONTHO */|#
  ( #x0db2 #x0e12 ) #|/*             Thai_thophuthao ฒ THAI CHARACTER THO PHUTHAO */|#
  ( #x0db3 #x0e13 ) #|/*                  Thai_nonen ณ THAI CHARACTER NO NEN */|#
  ( #x0db4 #x0e14 ) #|/*                  Thai_dodek ด THAI CHARACTER DO DEK */|#
  ( #x0db5 #x0e15 ) #|/*                  Thai_totao ต THAI CHARACTER TO TAO */|#
  ( #x0db6 #x0e16 ) #|/*               Thai_thothung ถ THAI CHARACTER THO THUNG */|#
  ( #x0db7 #x0e17 ) #|/*              Thai_thothahan ท THAI CHARACTER THO THAHAN */|#
  ( #x0db8 #x0e18 ) #|/*               Thai_thothong ธ THAI CHARACTER THO THONG */|#
  ( #x0db9 #x0e19 ) #|/*                   Thai_nonu น THAI CHARACTER NO NU */|#
  ( #x0dba #x0e1a ) #|/*               Thai_bobaimai บ THAI CHARACTER BO BAIMAI */|#
  ( #x0dbb #x0e1b ) #|/*                  Thai_popla ป THAI CHARACTER PO PLA */|#
  ( #x0dbc #x0e1c ) #|/*               Thai_phophung ผ THAI CHARACTER PHO PHUNG */|#
  ( #x0dbd #x0e1d ) #|/*                   Thai_fofa ฝ THAI CHARACTER FO FA */|#
  ( #x0dbe #x0e1e ) #|/*                Thai_phophan พ THAI CHARACTER PHO PHAN */|#
  ( #x0dbf #x0e1f ) #|/*                  Thai_fofan ฟ THAI CHARACTER FO FAN */|#
  ( #x0dc0 #x0e20 ) #|/*             Thai_phosamphao ภ THAI CHARACTER PHO SAMPHAO */|#
  ( #x0dc1 #x0e21 ) #|/*                   Thai_moma ม THAI CHARACTER MO MA */|#
  ( #x0dc2 #x0e22 ) #|/*                  Thai_yoyak ย THAI CHARACTER YO YAK */|#
  ( #x0dc3 #x0e23 ) #|/*                  Thai_rorua ร THAI CHARACTER RO RUA */|#
  ( #x0dc4 #x0e24 ) #|/*                     Thai_ru ฤ THAI CHARACTER RU */|#
  ( #x0dc5 #x0e25 ) #|/*                 Thai_loling ล THAI CHARACTER LO LING */|#
  ( #x0dc6 #x0e26 ) #|/*                     Thai_lu ฦ THAI CHARACTER LU */|#
  ( #x0dc7 #x0e27 ) #|/*                 Thai_wowaen ว THAI CHARACTER WO WAEN */|#
  ( #x0dc8 #x0e28 ) #|/*                 Thai_sosala ศ THAI CHARACTER SO SALA */|#
  ( #x0dc9 #x0e29 ) #|/*                 Thai_sorusi ษ THAI CHARACTER SO RUSI */|#
  ( #x0dca #x0e2a ) #|/*                  Thai_sosua ส THAI CHARACTER SO SUA */|#
  ( #x0dcb #x0e2b ) #|/*                  Thai_hohip ห THAI CHARACTER HO HIP */|#
  ( #x0dcc #x0e2c ) #|/*                Thai_lochula ฬ THAI CHARACTER LO CHULA */|#
  ( #x0dcd #x0e2d ) #|/*                   Thai_oang อ THAI CHARACTER O ANG */|#
  ( #x0dce #x0e2e ) #|/*               Thai_honokhuk ฮ THAI CHARACTER HO NOKHUK */|#
  ( #x0dcf #x0e2f ) #|/*              Thai_paiyannoi ฯ THAI CHARACTER PAIYANNOI */|#
  ( #x0dd0 #x0e30 ) #|/*                  Thai_saraa ะ THAI CHARACTER SARA A */|#
  ( #x0dd1 #x0e31 ) #|/*             Thai_maihanakat ั THAI CHARACTER MAI HAN-AKAT */|#
  ( #x0dd2 #x0e32 ) #|/*                 Thai_saraaa า THAI CHARACTER SARA AA */|#
  ( #x0dd3 #x0e33 ) #|/*                 Thai_saraam ำ THAI CHARACTER SARA AM */|#
  ( #x0dd4 #x0e34 ) #|/*                  Thai_sarai ิ THAI CHARACTER SARA I */|#
  ( #x0dd5 #x0e35 ) #|/*                 Thai_saraii ี THAI CHARACTER SARA II */|#
  ( #x0dd6 #x0e36 ) #|/*                 Thai_saraue ึ THAI CHARACTER SARA UE */|#
  ( #x0dd7 #x0e37 ) #|/*                Thai_sarauee ื THAI CHARACTER SARA UEE */|#
  ( #x0dd8 #x0e38 ) #|/*                  Thai_sarau ุ THAI CHARACTER SARA U */|#
  ( #x0dd9 #x0e39 ) #|/*                 Thai_sarauu ู THAI CHARACTER SARA UU */|#
  ( #x0dda #x0e3a ) #|/*                Thai_phinthu ฺ THAI CHARACTER PHINTHU */|#
  ( #x0ddf #x0e3f ) #|/*                   Thai_baht ฿ THAI CURRENCY SYMBOL BAHT */|#
  ( #x0de0 #x0e40 ) #|/*                  Thai_sarae เ THAI CHARACTER SARA E */|#
  ( #x0de1 #x0e41 ) #|/*                 Thai_saraae แ THAI CHARACTER SARA AE */|#
  ( #x0de2 #x0e42 ) #|/*                  Thai_sarao โ THAI CHARACTER SARA O */|#
  ( #x0de3 #x0e43 ) #|/*          Thai_saraaimaimuan ใ THAI CHARACTER SARA AI MAIMUAN */|#
  ( #x0de4 #x0e44 ) #|/*         Thai_saraaimaimalai ไ THAI CHARACTER SARA AI MAIMALAI */|#
  ( #x0de5 #x0e45 ) #|/*            Thai_lakkhangyao ๅ THAI CHARACTER LAKKHANGYAO */|#
  ( #x0de6 #x0e46 ) #|/*               Thai_maiyamok ๆ THAI CHARACTER MAIYAMOK */|#
  ( #x0de7 #x0e47 ) #|/*              Thai_maitaikhu ็ THAI CHARACTER MAITAIKHU */|#
  ( #x0de8 #x0e48 ) #|/*                  Thai_maiek ่ THAI CHARACTER MAI EK */|#
  ( #x0de9 #x0e49 ) #|/*                 Thai_maitho ้ THAI CHARACTER MAI THO */|#
  ( #x0dea #x0e4a ) #|/*                 Thai_maitri ๊ THAI CHARACTER MAI TRI */|#
  ( #x0deb #x0e4b ) #|/*            Thai_maichattawa ๋ THAI CHARACTER MAI CHATTAWA */|#
  ( #x0dec #x0e4c ) #|/*            Thai_thanthakhat ์ THAI CHARACTER THANTHAKHAT */|#
  ( #x0ded #x0e4d ) #|/*               Thai_nikhahit ํ THAI CHARACTER NIKHAHIT */|#
  ( #x0df0 #x0e50 ) #|/*                 Thai_leksun ๐ THAI DIGIT ZERO */|#
  ( #x0df1 #x0e51 ) #|/*                Thai_leknung ๑ THAI DIGIT ONE */|#
  ( #x0df2 #x0e52 ) #|/*                Thai_leksong ๒ THAI DIGIT TWO */|#
  ( #x0df3 #x0e53 ) #|/*                 Thai_leksam ๓ THAI DIGIT THREE */|#
  ( #x0df4 #x0e54 ) #|/*                  Thai_leksi ๔ THAI DIGIT FOUR */|#
  ( #x0df5 #x0e55 ) #|/*                  Thai_lekha ๕ THAI DIGIT FIVE */|#
  ( #x0df6 #x0e56 ) #|/*                 Thai_lekhok ๖ THAI DIGIT SIX */|#
  ( #x0df7 #x0e57 ) #|/*                Thai_lekchet ๗ THAI DIGIT SEVEN */|#
  ( #x0df8 #x0e58 ) #|/*                Thai_lekpaet ๘ THAI DIGIT EIGHT */|#
  ( #x0df9 #x0e59 ) #|/*                 Thai_lekkao ๙ THAI DIGIT NINE */|#
  ( #x0ed4 #x11a8 ) #|/*             Hangul_J_Kiyeog ᆨ HANGUL JONGSEONG KIYEOK */|#
  ( #x0ed5 #x11a9 ) #|/*        Hangul_J_SsangKiyeog ᆩ HANGUL JONGSEONG SSANGKIYEOK */|#
  ( #x0ed6 #x11aa ) #|/*         Hangul_J_KiyeogSios ᆪ HANGUL JONGSEONG KIYEOK-SIOS */|#
  ( #x0ed7 #x11ab ) #|/*              Hangul_J_Nieun ᆫ HANGUL JONGSEONG NIEUN */|#
  ( #x0ed8 #x11ac ) #|/*         Hangul_J_NieunJieuj ᆬ HANGUL JONGSEONG NIEUN-CIEUC */|#
  ( #x0ed9 #x11ad ) #|/*         Hangul_J_NieunHieuh ᆭ HANGUL JONGSEONG NIEUN-HIEUH */|#
  ( #x0eda #x11ae ) #|/*             Hangul_J_Dikeud ᆮ HANGUL JONGSEONG TIKEUT */|#
  ( #x0edb #x11af ) #|/*              Hangul_J_Rieul ᆯ HANGUL JONGSEONG RIEUL */|#
  ( #x0edc #x11b0 ) #|/*        Hangul_J_RieulKiyeog ᆰ HANGUL JONGSEONG RIEUL-KIYEOK */|#
  ( #x0edd #x11b1 ) #|/*         Hangul_J_RieulMieum ᆱ HANGUL JONGSEONG RIEUL-MIEUM */|#
  ( #x0ede #x11b2 ) #|/*         Hangul_J_RieulPieub ᆲ HANGUL JONGSEONG RIEUL-PIEUP */|#
  ( #x0edf #x11b3 ) #|/*          Hangul_J_RieulSios ᆳ HANGUL JONGSEONG RIEUL-SIOS */|#
  ( #x0ee0 #x11b4 ) #|/*         Hangul_J_RieulTieut ᆴ HANGUL JONGSEONG RIEUL-THIEUTH */|#
  ( #x0ee1 #x11b5 ) #|/*        Hangul_J_RieulPhieuf ᆵ HANGUL JONGSEONG RIEUL-PHIEUPH */|#
  ( #x0ee2 #x11b6 ) #|/*         Hangul_J_RieulHieuh ᆶ HANGUL JONGSEONG RIEUL-HIEUH */|#
  ( #x0ee3 #x11b7 ) #|/*              Hangul_J_Mieum ᆷ HANGUL JONGSEONG MIEUM */|#
  ( #x0ee4 #x11b8 ) #|/*              Hangul_J_Pieub ᆸ HANGUL JONGSEONG PIEUP */|#
  ( #x0ee5 #x11b9 ) #|/*          Hangul_J_PieubSios ᆹ HANGUL JONGSEONG PIEUP-SIOS */|#
  ( #x0ee6 #x11ba ) #|/*               Hangul_J_Sios ᆺ HANGUL JONGSEONG SIOS */|#
  ( #x0ee7 #x11bb ) #|/*          Hangul_J_SsangSios ᆻ HANGUL JONGSEONG SSANGSIOS */|#
  ( #x0ee8 #x11bc ) #|/*              Hangul_J_Ieung ᆼ HANGUL JONGSEONG IEUNG */|#
  ( #x0ee9 #x11bd ) #|/*              Hangul_J_Jieuj ᆽ HANGUL JONGSEONG CIEUC */|#
  ( #x0eea #x11be ) #|/*              Hangul_J_Cieuc ᆾ HANGUL JONGSEONG CHIEUCH */|#
  ( #x0eeb #x11bf ) #|/*             Hangul_J_Khieuq ᆿ HANGUL JONGSEONG KHIEUKH */|#
  ( #x0eec #x11c0 ) #|/*              Hangul_J_Tieut ᇀ HANGUL JONGSEONG THIEUTH */|#
  ( #x0eed #x11c1 ) #|/*             Hangul_J_Phieuf ᇁ HANGUL JONGSEONG PHIEUPH */|#
  ( #x0eee #x11c2 ) #|/*              Hangul_J_Hieuh ᇂ HANGUL JONGSEONG HIEUH */|#
  ( #x0ef8 #x11eb ) #|/*            Hangul_J_PanSios ᇫ HANGUL JONGSEONG PANSIOS */|#
  ( #x0ef9 #x11f0 ) #|/*  Hangul_J_KkogjiDalrinIeung ᇰ HANGUL JONGSEONG YESIEUNG */|#
  ( #x0efa #x11f9 ) #|/*        Hangul_J_YeorinHieuh ᇹ HANGUL JONGSEONG YEORINHIEUH */|#
  ( #x0aa2 #x2002 ) #|/*                     enspace   EN SPACE */|#
  ( #x0aa1 #x2003 ) #|/*                     emspace   EM SPACE */|#
  ( #x0aa3 #x2004 ) #|/*                    em3space   THREE-PER-EM SPACE */|#
  ( #x0aa4 #x2005 ) #|/*                    em4space   FOUR-PER-EM SPACE */|#
  ( #x0aa5 #x2007 ) #|/*                  digitspace   FIGURE SPACE */|#
  ( #x0aa6 #x2008 ) #|/*                  punctspace   PUNCTUATION SPACE */|#
  ( #x0aa7 #x2009 ) #|/*                   thinspace   THIN SPACE */|#
  ( #x0aa8 #x200a ) #|/*                   hairspace   HAIR SPACE */|#
  ( #x0abb #x2012 ) #|/*                     figdash ‒ FIGURE DASH */|#
  ( #x0aaa #x2013 ) #|/*                      endash – EN DASH */|#
  ( #x0aa9 #x2014 ) #|/*                      emdash — EM DASH */|#
  ( #x07af #x2015 ) #|/*              Greek_horizbar ― HORIZONTAL BAR */|#
  ( #x0cdf #x2017 ) #|/*        hebrew_doublelowline ‗ DOUBLE LOW LINE */|#
  ( #x0ad0 #x2018 ) #|/*         leftsinglequotemark ‘ LEFT SINGLE QUOTATION MARK */|#
  ( #x0ad1 #x2019 ) #|/*        rightsinglequotemark ’ RIGHT SINGLE QUOTATION MARK */|#
  ( #x0afd #x201a ) #|/*          singlelowquotemark ‚ SINGLE LOW-9 QUOTATION MARK */|#
  ( #x0ad2 #x201c ) #|/*         leftdoublequotemark “ LEFT DOUBLE QUOTATION MARK */|#
  ( #x0ad3 #x201d ) #|/*        rightdoublequotemark ” RIGHT DOUBLE QUOTATION MARK */|#
  ( #x0afe #x201e ) #|/*          doublelowquotemark „ DOUBLE LOW-9 QUOTATION MARK */|#
  ( #x0af1 #x2020 ) #|/*                      dagger † DAGGER */|#
  ( #x0af2 #x2021 ) #|/*                doubledagger ‡ DOUBLE DAGGER */|#
  ( #x0ae6 #x2022 ) #|/*          enfilledcircbullet • BULLET */|#
  ( #x0aaf #x2025 ) #|/*             doubbaselinedot ‥ TWO DOT LEADER */|#
  ( #x0aae #x2026 ) #|/*                    ellipsis … HORIZONTAL ELLIPSIS */|#
  ( #x0ad5 #x2030 ) #|/*                    permille ‰ PER MILLE SIGN */|#
  ( #x0ad6 #x2032 ) #|/*                     minutes ′ PRIME */|#
  ( #x0ad7 #x2033 ) #|/*                     seconds ″ DOUBLE PRIME */|#
  ( #x0afc #x2038 ) #|/*                       caret ‸ CARET */|#
  ( #x047e #x203e ) #|/*                    overline ‾ OVERLINE */|#
  ( #x20a0 #x20a0 ) #|/*                     EcuSign ₠ EURO-CURRENCY SIGN */|#
  ( #x20a1 #x20a1 ) #|/*                   ColonSign ₡ COLON SIGN */|#
  ( #x20a2 #x20a2 ) #|/*                CruzeiroSign ₢ CRUZEIRO SIGN */|#
  ( #x20a3 #x20a3 ) #|/*                  FFrancSign ₣ FRENCH FRANC SIGN */|#
  ( #x20a4 #x20a4 ) #|/*                    LiraSign ₤ LIRA SIGN */|#
  ( #x20a5 #x20a5 ) #|/*                    MillSign ₥ MILL SIGN */|#
  ( #x20a6 #x20a6 ) #|/*                   NairaSign ₦ NAIRA SIGN */|#
  ( #x20a7 #x20a7 ) #|/*                  PesetaSign ₧ PESETA SIGN */|#
  ( #x20a8 #x20a8 ) #|/*                   RupeeSign ₨ RUPEE SIGN */|#
  ( #x0eff #x20a9 ) #|/*                  Korean_Won ₩ WON SIGN */|#
  ( #x20a9 #x20a9 ) #|/*                     WonSign ₩ WON SIGN */|#
  ( #x20aa #x20aa ) #|/*               NewSheqelSign ₪ NEW SHEQEL SIGN */|#
  ( #x20ab #x20ab ) #|/*                    DongSign ₫ DONG SIGN */|#
  ( #x20ac #x20ac ) #|/*                    EuroSign € EURO SIGN */|#
  ( #x0ab8 #x2105 ) #|/*                      careof ℅ CARE OF */|#
  ( #x06b0 #x2116 ) #|/*                  numerosign № NUMERO SIGN */|#
  ( #x0afb #x2117 ) #|/*         phonographcopyright ℗ SOUND RECORDING COPYRIGHT */|#
  ( #x0ad4 #x211e ) #|/*                prescription ℞ PRESCRIPTION TAKE */|#
  ( #x0ac9 #x2122 ) #|/*                   trademark ™ TRADE MARK SIGN */|#
  ( #x0ab0 #x2153 ) #|/*                    onethird ⅓ VULGAR FRACTION ONE THIRD */|#
  ( #x0ab1 #x2154 ) #|/*                   twothirds ⅔ VULGAR FRACTION TWO THIRDS */|#
  ( #x0ab2 #x2155 ) #|/*                    onefifth ⅕ VULGAR FRACTION ONE FIFTH */|#
  ( #x0ab3 #x2156 ) #|/*                   twofifths ⅖ VULGAR FRACTION TWO FIFTHS */|#
  ( #x0ab4 #x2157 ) #|/*                 threefifths ⅗ VULGAR FRACTION THREE FIFTHS */|#
  ( #x0ab5 #x2158 ) #|/*                  fourfifths ⅘ VULGAR FRACTION FOUR FIFTHS */|#
  ( #x0ab6 #x2159 ) #|/*                    onesixth ⅙ VULGAR FRACTION ONE SIXTH */|#
  ( #x0ab7 #x215a ) #|/*                  fivesixths ⅚ VULGAR FRACTION FIVE SIXTHS */|#
  ( #x0ac3 #x215b ) #|/*                   oneeighth ⅛ VULGAR FRACTION ONE EIGHTH */|#
  ( #x0ac4 #x215c ) #|/*                threeeighths ⅜ VULGAR FRACTION THREE EIGHTHS */|#
  ( #x0ac5 #x215d ) #|/*                 fiveeighths ⅝ VULGAR FRACTION FIVE EIGHTHS */|#
  ( #x0ac6 #x215e ) #|/*                seveneighths ⅞ VULGAR FRACTION SEVEN EIGHTHS */|#
  ( #x08fb #x2190 ) #|/*                   leftarrow ← LEFTWARDS ARROW */|#
  ( #x08fc #x2191 ) #|/*                     uparrow ↑ UPWARDS ARROW */|#
  ( #x08fd #x2192 ) #|/*                  rightarrow → RIGHTWARDS ARROW */|#
  ( #x08fe #x2193 ) #|/*                   downarrow ↓ DOWNWARDS ARROW */|#
  ( #x08ce #x21d2 ) #|/*                     implies ⇒ RIGHTWARDS DOUBLE ARROW */|#
  ( #x08cd #x21d4 ) #|/*                    ifonlyif ⇔ LEFT RIGHT DOUBLE ARROW */|#
  ( #x08ef #x2202 ) #|/*           partialderivative ∂ PARTIAL DIFFERENTIAL */|#
  ( #x08c5 #x2207 ) #|/*                       nabla ∇ NABLA */|#
  ( #x0bca #x2218 ) #|/*                         jot ∘ RING OPERATOR */|#
  ( #x08d6 #x221a ) #|/*                     radical √ SQUARE ROOT */|#
  ( #x08c1 #x221d ) #|/*                   variation ∝ PROPORTIONAL TO */|#
  ( #x08c2 #x221e ) #|/*                    infinity ∞ INFINITY */|#
  ( #x08de #x2227 ) #|/*                  logicaland ∧ LOGICAL AND */|#
  ( #x0ba9 #x2227 ) #|/*                     upcaret ∧ LOGICAL AND */|#
  ( #x08df #x2228 ) #|/*                   logicalor ∨ LOGICAL OR */|#
  ( #x0ba8 #x2228 ) #|/*                   downcaret ∨ LOGICAL OR */|#
  ( #x08dc #x2229 ) #|/*                intersection ∩ INTERSECTION */|#
  ( #x0bc3 #x2229 ) #|/*                      upshoe ∩ INTERSECTION */|#
  ( #x08dd #x222a ) #|/*                       union ∪ UNION */|#
  ( #x0bd6 #x222a ) #|/*                    downshoe ∪ UNION */|#
  ( #x08bf #x222b ) #|/*                    integral ∫ INTEGRAL */|#
  ( #x08c0 #x2234 ) #|/*                   therefore ∴ THEREFORE */|#
  ( #x08c8 #x223c ) #|/*                 approximate ∼ TILDE OPERATOR */|#
  ( #x08c9 #x2243 ) #|/*                similarequal ≃ ASYMPTOTICALLY EQUAL TO */|#
  ( #x08bd #x2260 ) #|/*                    notequal ≠ NOT EQUAL TO */|#
  ( #x08cf #x2261 ) #|/*                   identical ≡ IDENTICAL TO */|#
  ( #x08bc #x2264 ) #|/*               lessthanequal ≤ LESS-THAN OR EQUAL TO */|#
  ( #x08be #x2265 ) #|/*            greaterthanequal ≥ GREATER-THAN OR EQUAL TO */|#
  ( #x08da #x2282 ) #|/*                  includedin ⊂ SUBSET OF */|#
  ( #x0bda #x2282 ) #|/*                    leftshoe ⊂ SUBSET OF */|#
  ( #x08db #x2283 ) #|/*                    includes ⊃ SUPERSET OF */|#
  ( #x0bd8 #x2283 ) #|/*                   rightshoe ⊃ SUPERSET OF */|#
  ( #x0bfc #x22a2 ) #|/*                   righttack ⊢ RIGHT TACK */|#
  ( #x0bdc #x22a3 ) #|/*                    lefttack ⊣ LEFT TACK */|#
  ( #x0bc2 #x22a4 ) #|/*                    downtack ⊤ DOWN TACK */|#
  ( #x0bce #x22a5 ) #|/*                      uptack ⊥ UP TACK */|#
  ( #x0bd3 #x2308 ) #|/*                     upstile ⌈ LEFT CEILING */|#
  ( #x0bc4 #x230a ) #|/*                   downstile ⌊ LEFT FLOOR */|#
  ( #x0afa #x2315 ) #|/*           telephonerecorder ⌕ TELEPHONE RECORDER */|#
  ( #x08a4 #x2320 ) #|/*                 topintegral ⌠ TOP HALF INTEGRAL */|#
  ( #x08a5 #x2321 ) #|/*                 botintegral ⌡ BOTTOM HALF INTEGRAL */|#
  ( #x0bcc #x2395 ) #|/*                        quad ⎕ APL FUNCTIONAL SYMBOL QUAD (Unicode 3.0) */|#
  ( #x08a7 #x23a1 ) #|/*            topleftsqbracket ⎡ ??? */|#
  ( #x08a8 #x23a3 ) #|/*            botleftsqbracket ⎣ ??? */|#
  ( #x08a9 #x23a4 ) #|/*           toprightsqbracket ⎤ ??? */|#
  ( #x08aa #x23a6 ) #|/*           botrightsqbracket ⎦ ??? */|#
  ( #x08ab #x239b ) #|/*               topleftparens ⎛ ??? */|#
  ( #x08ac #x239d ) #|/*               botleftparens ⎝ ??? */|#
  ( #x08ad #x239e ) #|/*              toprightparens ⎞ ??? */|#
  ( #x08ae #x23a0 ) #|/*              botrightparens ⎠ ??? */|#
  ( #x08af #x23a8 ) #|/*        leftmiddlecurlybrace ⎨ ??? */|#
  ( #x08b0 #x23ac ) #|/*       rightmiddlecurlybrace ⎬ ??? */|#
  ( #x08a1 #x23b7 ) #|/*                 leftradical ⎷ ??? */|#
  ( #x09ef #x23ba ) #|/*              horizlinescan1 ⎺ HORIZONTAL SCAN LINE-1 (Unicode 3.2 draft) */|#
  ( #x09f0 #x23bb ) #|/*              horizlinescan3 ⎻ HORIZONTAL SCAN LINE-3 (Unicode 3.2 draft) */|#
  ( #x09f2 #x23bc ) #|/*              horizlinescan7 ⎼ HORIZONTAL SCAN LINE-7 (Unicode 3.2 draft) */|#
  ( #x09f3 #x23bd ) #|/*              horizlinescan9 ⎽ HORIZONTAL SCAN LINE-9 (Unicode 3.2 draft) */|#
  ( #x09e2 #x2409 ) #|/*                          ht ␉ SYMBOL FOR HORIZONTAL TABULATION */|#
  ( #x09e5 #x240a ) #|/*                          lf ␊ SYMBOL FOR LINE FEED */|#
  ( #x09e9 #x240b ) #|/*                          vt ␋ SYMBOL FOR VERTICAL TABULATION */|#
  ( #x09e3 #x240c ) #|/*                          ff ␌ SYMBOL FOR FORM FEED */|#
  ( #x09e4 #x240d ) #|/*                          cr ␍ SYMBOL FOR CARRIAGE RETURN */|#
  ( #x0aac #x2423 ) #|/*                 signifblank ␣ OPEN BOX */|#
  ( #x09e8 #x2424 ) #|/*                          nl ␤ SYMBOL FOR NEWLINE */|#
  ( #x09f1 #x2500 ) #|/*              horizlinescan5 ─ BOX DRAWINGS LIGHT HORIZONTAL */|#
  ( #x08a6 #x2502 ) #|/*               vertconnector │ BOX DRAWINGS LIGHT VERTICAL */|#
  ( #x09f8 #x2502 ) #|/*                     vertbar │ BOX DRAWINGS LIGHT VERTICAL */|#
  ( #x09ec #x250c ) #|/*                upleftcorner ┌ BOX DRAWINGS LIGHT DOWN AND RIGHT */|#
  ( #x09eb #x2510 ) #|/*               uprightcorner ┐ BOX DRAWINGS LIGHT DOWN AND LEFT */|#
  ( #x09ed #x2514 ) #|/*               lowleftcorner └ BOX DRAWINGS LIGHT UP AND RIGHT */|#
  ( #x09ea #x2518 ) #|/*              lowrightcorner ┘ BOX DRAWINGS LIGHT UP AND LEFT */|#
  ( #x09f4 #x251c ) #|/*                       leftt ├ BOX DRAWINGS LIGHT VERTICAL AND RIGHT */|#
  ( #x09f5 #x2524 ) #|/*                      rightt ┤ BOX DRAWINGS LIGHT VERTICAL AND LEFT */|#
  ( #x09f7 #x252c ) #|/*                        topt ┬ BOX DRAWINGS LIGHT DOWN AND HORIZONTAL */|#
  ( #x09f6 #x2534 ) #|/*                        bott ┴ BOX DRAWINGS LIGHT UP AND HORIZONTAL */|#
  ( #x09ee #x253c ) #|/*               crossinglines ┼ BOX DRAWINGS LIGHT VERTICAL AND HORIZONTAL */|#
  ( #x09e1 #x2592 ) #|/*                checkerboard ▒ MEDIUM SHADE */|#
  ( #x0adf #x25a0 ) #|/*                emfilledrect ■ BLACK SQUARE */|#
  ( #x0ae7 #x25aa ) #|/*            enfilledsqbullet ▪ BLACK SMALL SQUARE */|#
  ( #x0ae1 #x25ab ) #|/*          enopensquarebullet ▫ WHITE SMALL SQUARE */|#
  ( #x0adb #x25ac ) #|/*            filledrectbullet ▬ BLACK RECTANGLE */|#
  ( #x0ae2 #x25ad ) #|/*              openrectbullet ▭ WHITE RECTANGLE */|#
  ( #x0acf #x25af ) #|/*             emopenrectangle ▯ WHITE VERTICAL RECTANGLE */|#
  ( #x0ae8 #x25b2 ) #|/*           filledtribulletup ▲ BLACK UP-POINTING TRIANGLE */|#
  ( #x0ae3 #x25b3 ) #|/*             opentribulletup △ WHITE UP-POINTING TRIANGLE */|#
  ( #x0add #x25b6 ) #|/*        filledrighttribullet ▶ BLACK RIGHT-POINTING TRIANGLE */|#
  ( #x0acd #x25b7 ) #|/*           rightopentriangle ▷ WHITE RIGHT-POINTING TRIANGLE */|#
  ( #x0ae9 #x25bc ) #|/*         filledtribulletdown ▼ BLACK DOWN-POINTING TRIANGLE */|#
  ( #x0ae4 #x25bd ) #|/*           opentribulletdown ▽ WHITE DOWN-POINTING TRIANGLE */|#
  ( #x0adc #x25c0 ) #|/*         filledlefttribullet ◀ BLACK LEFT-POINTING TRIANGLE */|#
  ( #x0acc #x25c1 ) #|/*            leftopentriangle ◁ WHITE LEFT-POINTING TRIANGLE */|#
  ( #x09e0 #x25c6 ) #|/*                soliddiamond ◆ BLACK DIAMOND */|#
  ( #x0ace #x25cb ) #|/*                emopencircle ○ WHITE CIRCLE */|#
  ( #x0bcf #x25cb ) #|/*                      circle ○ WHITE CIRCLE */|#
  ( #x0ade #x25cf ) #|/*              emfilledcircle ● BLACK CIRCLE */|#
  ( #x0ae0 #x25e6 ) #|/*            enopencircbullet ◦ WHITE BULLET */|#
  ( #x0ae5 #x2606 ) #|/*                    openstar ☆ WHITE STAR */|#
  ( #x0af9 #x260e ) #|/*                   telephone ☎ BLACK TELEPHONE */|#
  ( #x0aca #x2613 ) #|/*               signaturemark ☓ SALTIRE */|#
  ( #x0aea #x261c ) #|/*                 leftpointer ☜ WHITE LEFT POINTING INDEX */|#
  ( #x0aeb #x261e ) #|/*                rightpointer ☞ WHITE RIGHT POINTING INDEX */|#
  ( #x0af8 #x2640 ) #|/*                femalesymbol ♀ FEMALE SIGN */|#
  ( #x0af7 #x2642 ) #|/*                  malesymbol ♂ MALE SIGN */|#
  ( #x0aec #x2663 ) #|/*                        club ♣ BLACK CLUB SUIT */|#
  ( #x0aee #x2665 ) #|/*                       heart ♥ BLACK HEART SUIT */|#
  ( #x0aed #x2666 ) #|/*                     diamond ♦ BLACK DIAMOND SUIT */|#
  ( #x0af6 #x266d ) #|/*                 musicalflat ♭ MUSIC FLAT SIGN */|#
  ( #x0af5 #x266f ) #|/*                musicalsharp ♯ MUSIC SHARP SIGN */|#
  ( #x0af3 #x2713 ) #|/*                   checkmark ✓ CHECK MARK */|#
  ( #x0af4 #x2717 ) #|/*                 ballotcross ✗ BALLOT X */|#
  ( #x0ad9 #x271d ) #|/*                  latincross ✝ LATIN CROSS */|#
  ( #x0af0 #x2720 ) #|/*                maltesecross ✠ MALTESE CROSS */|#
  ( #x0abc #x27e8 ) #|/*            leftanglebracket ⟨ MATHEMATICAL LEFT ANGLE BRACKET */|#
  ( #x0abe #x27e9 ) #|/*           rightanglebracket ⟩ MATHEMATICAL RIGHT ANGLE BRACKET */|#
  ( #x04a4 #x3001 ) #|/*                  kana_comma 、 IDEOGRAPHIC COMMA */|#
  ( #x04a1 #x3002 ) #|/*               kana_fullstop 。 IDEOGRAPHIC FULL STOP */|#
  ( #x04a2 #x300c ) #|/*         kana_openingbracket 「 LEFT CORNER BRACKET */|#
  ( #x04a3 #x300d ) #|/*         kana_closingbracket 」 RIGHT CORNER BRACKET */|#
  ( #x04de #x309b ) #|/*                 voicedsound ゛ KATAKANA-HIRAGANA VOICED SOUND MARK */|#
  ( #x04df #x309c ) #|/*             semivoicedsound ゜ KATAKANA-HIRAGANA SEMI-VOICED SOUND MARK */|#
  ( #x04a7 #x30a1 ) #|/*                      kana_a ァ KATAKANA LETTER SMALL A */|#
  ( #x04b1 #x30a2 ) #|/*                      kana_A ア KATAKANA LETTER A */|#
  ( #x04a8 #x30a3 ) #|/*                      kana_i ィ KATAKANA LETTER SMALL I */|#
  ( #x04b2 #x30a4 ) #|/*                      kana_I イ KATAKANA LETTER I */|#
  ( #x04a9 #x30a5 ) #|/*                      kana_u ゥ KATAKANA LETTER SMALL U */|#
  ( #x04b3 #x30a6 ) #|/*                      kana_U ウ KATAKANA LETTER U */|#
  ( #x04aa #x30a7 ) #|/*                      kana_e ェ KATAKANA LETTER SMALL E */|#
  ( #x04b4 #x30a8 ) #|/*                      kana_E エ KATAKANA LETTER E */|#
  ( #x04ab #x30a9 ) #|/*                      kana_o ォ KATAKANA LETTER SMALL O */|#
  ( #x04b5 #x30aa ) #|/*                      kana_O オ KATAKANA LETTER O */|#
  ( #x04b6 #x30ab ) #|/*                     kana_KA カ KATAKANA LETTER KA */|#
  ( #x04b7 #x30ad ) #|/*                     kana_KI キ KATAKANA LETTER KI */|#
  ( #x04b8 #x30af ) #|/*                     kana_KU ク KATAKANA LETTER KU */|#
  ( #x04b9 #x30b1 ) #|/*                     kana_KE ケ KATAKANA LETTER KE */|#
  ( #x04ba #x30b3 ) #|/*                     kana_KO コ KATAKANA LETTER KO */|#
  ( #x04bb #x30b5 ) #|/*                     kana_SA サ KATAKANA LETTER SA */|#
  ( #x04bc #x30b7 ) #|/*                    kana_SHI シ KATAKANA LETTER SI */|#
  ( #x04bd #x30b9 ) #|/*                     kana_SU ス KATAKANA LETTER SU */|#
  ( #x04be #x30bb ) #|/*                     kana_SE セ KATAKANA LETTER SE */|#
  ( #x04bf #x30bd ) #|/*                     kana_SO ソ KATAKANA LETTER SO */|#
  ( #x04c0 #x30bf ) #|/*                     kana_TA タ KATAKANA LETTER TA */|#
  ( #x04c1 #x30c1 ) #|/*                    kana_CHI チ KATAKANA LETTER TI */|#
  ( #x04af #x30c3 ) #|/*                    kana_tsu ッ KATAKANA LETTER SMALL TU */|#
  ( #x04c2 #x30c4 ) #|/*                    kana_TSU ツ KATAKANA LETTER TU */|#
  ( #x04c3 #x30c6 ) #|/*                     kana_TE テ KATAKANA LETTER TE */|#
  ( #x04c4 #x30c8 ) #|/*                     kana_TO ト KATAKANA LETTER TO */|#
  ( #x04c5 #x30ca ) #|/*                     kana_NA ナ KATAKANA LETTER NA */|#
  ( #x04c6 #x30cb ) #|/*                     kana_NI ニ KATAKANA LETTER NI */|#
  ( #x04c7 #x30cc ) #|/*                     kana_NU ヌ KATAKANA LETTER NU */|#
  ( #x04c8 #x30cd ) #|/*                     kana_NE ネ KATAKANA LETTER NE */|#
  ( #x04c9 #x30ce ) #|/*                     kana_NO ノ KATAKANA LETTER NO */|#
  ( #x04ca #x30cf ) #|/*                     kana_HA ハ KATAKANA LETTER HA */|#
  ( #x04cb #x30d2 ) #|/*                     kana_HI ヒ KATAKANA LETTER HI */|#
  ( #x04cc #x30d5 ) #|/*                     kana_FU フ KATAKANA LETTER HU */|#
  ( #x04cd #x30d8 ) #|/*                     kana_HE ヘ KATAKANA LETTER HE */|#
  ( #x04ce #x30db ) #|/*                     kana_HO ホ KATAKANA LETTER HO */|#
  ( #x04cf #x30de ) #|/*                     kana_MA マ KATAKANA LETTER MA */|#
  ( #x04d0 #x30df ) #|/*                     kana_MI ミ KATAKANA LETTER MI */|#
  ( #x04d1 #x30e0 ) #|/*                     kana_MU ム KATAKANA LETTER MU */|#
  ( #x04d2 #x30e1 ) #|/*                     kana_ME メ KATAKANA LETTER ME */|#
  ( #x04d3 #x30e2 ) #|/*                     kana_MO モ KATAKANA LETTER MO */|#
  ( #x04ac #x30e3 ) #|/*                     kana_ya ャ KATAKANA LETTER SMALL YA */|#
  ( #x04d4 #x30e4 ) #|/*                     kana_YA ヤ KATAKANA LETTER YA */|#
  ( #x04ad #x30e5 ) #|/*                     kana_yu ュ KATAKANA LETTER SMALL YU */|#
  ( #x04d5 #x30e6 ) #|/*                     kana_YU ユ KATAKANA LETTER YU */|#
  ( #x04ae #x30e7 ) #|/*                     kana_yo ョ KATAKANA LETTER SMALL YO */|#
  ( #x04d6 #x30e8 ) #|/*                     kana_YO ヨ KATAKANA LETTER YO */|#
  ( #x04d7 #x30e9 ) #|/*                     kana_RA ラ KATAKANA LETTER RA */|#
  ( #x04d8 #x30ea ) #|/*                     kana_RI リ KATAKANA LETTER RI */|#
  ( #x04d9 #x30eb ) #|/*                     kana_RU ル KATAKANA LETTER RU */|#
  ( #x04da #x30ec ) #|/*                     kana_RE レ KATAKANA LETTER RE */|#
  ( #x04db #x30ed ) #|/*                     kana_RO ロ KATAKANA LETTER RO */|#
  ( #x04dc #x30ef ) #|/*                     kana_WA ワ KATAKANA LETTER WA */|#
  ( #x04a6 #x30f2 ) #|/*                     kana_WO ヲ KATAKANA LETTER WO */|#
  ( #x04dd #x30f3 ) #|/*                      kana_N ン KATAKANA LETTER N */|#
  ( #x04a5 #x30fb ) #|/*            kana_conjunctive ・ KATAKANA MIDDLE DOT */|#
  ( #x04b0 #x30fc ) #|/*              prolongedsound ー KATAKANA-HIRAGANA PROLONGED SOUND MARK */|#
  ( #x0ea1 #x3131 ) #|/*               Hangul_Kiyeog ㄱ HANGUL LETTER KIYEOK */|#
  ( #x0ea2 #x3132 ) #|/*          Hangul_SsangKiyeog ㄲ HANGUL LETTER SSANGKIYEOK */|#
  ( #x0ea3 #x3133 ) #|/*           Hangul_KiyeogSios ㄳ HANGUL LETTER KIYEOK-SIOS */|#
  ( #x0ea4 #x3134 ) #|/*                Hangul_Nieun ㄴ HANGUL LETTER NIEUN */|#
  ( #x0ea5 #x3135 ) #|/*           Hangul_NieunJieuj ㄵ HANGUL LETTER NIEUN-CIEUC */|#
  ( #x0ea6 #x3136 ) #|/*           Hangul_NieunHieuh ㄶ HANGUL LETTER NIEUN-HIEUH */|#
  ( #x0ea7 #x3137 ) #|/*               Hangul_Dikeud ㄷ HANGUL LETTER TIKEUT */|#
  ( #x0ea8 #x3138 ) #|/*          Hangul_SsangDikeud ㄸ HANGUL LETTER SSANGTIKEUT */|#
  ( #x0ea9 #x3139 ) #|/*                Hangul_Rieul ㄹ HANGUL LETTER RIEUL */|#
  ( #x0eaa #x313a ) #|/*          Hangul_RieulKiyeog ㄺ HANGUL LETTER RIEUL-KIYEOK */|#
  ( #x0eab #x313b ) #|/*           Hangul_RieulMieum ㄻ HANGUL LETTER RIEUL-MIEUM */|#
  ( #x0eac #x313c ) #|/*           Hangul_RieulPieub ㄼ HANGUL LETTER RIEUL-PIEUP */|#
  ( #x0ead #x313d ) #|/*            Hangul_RieulSios ㄽ HANGUL LETTER RIEUL-SIOS */|#
  ( #x0eae #x313e ) #|/*           Hangul_RieulTieut ㄾ HANGUL LETTER RIEUL-THIEUTH */|#
  ( #x0eaf #x313f ) #|/*          Hangul_RieulPhieuf ㄿ HANGUL LETTER RIEUL-PHIEUPH */|#
  ( #x0eb0 #x3140 ) #|/*           Hangul_RieulHieuh ㅀ HANGUL LETTER RIEUL-HIEUH */|#
  ( #x0eb1 #x3141 ) #|/*                Hangul_Mieum ㅁ HANGUL LETTER MIEUM */|#
  ( #x0eb2 #x3142 ) #|/*                Hangul_Pieub ㅂ HANGUL LETTER PIEUP */|#
  ( #x0eb3 #x3143 ) #|/*           Hangul_SsangPieub ㅃ HANGUL LETTER SSANGPIEUP */|#
  ( #x0eb4 #x3144 ) #|/*            Hangul_PieubSios ㅄ HANGUL LETTER PIEUP-SIOS */|#
  ( #x0eb5 #x3145 ) #|/*                 Hangul_Sios ㅅ HANGUL LETTER SIOS */|#
  ( #x0eb6 #x3146 ) #|/*            Hangul_SsangSios ㅆ HANGUL LETTER SSANGSIOS */|#
  ( #x0eb7 #x3147 ) #|/*                Hangul_Ieung ㅇ HANGUL LETTER IEUNG */|#
  ( #x0eb8 #x3148 ) #|/*                Hangul_Jieuj ㅈ HANGUL LETTER CIEUC */|#
  ( #x0eb9 #x3149 ) #|/*           Hangul_SsangJieuj ㅉ HANGUL LETTER SSANGCIEUC */|#
  ( #x0eba #x314a ) #|/*                Hangul_Cieuc ㅊ HANGUL LETTER CHIEUCH */|#
  ( #x0ebb #x314b ) #|/*               Hangul_Khieuq ㅋ HANGUL LETTER KHIEUKH */|#
  ( #x0ebc #x314c ) #|/*                Hangul_Tieut ㅌ HANGUL LETTER THIEUTH */|#
  ( #x0ebd #x314d ) #|/*               Hangul_Phieuf ㅍ HANGUL LETTER PHIEUPH */|#
  ( #x0ebe #x314e ) #|/*                Hangul_Hieuh ㅎ HANGUL LETTER HIEUH */|#
  ( #x0ebf #x314f ) #|/*                    Hangul_A ㅏ HANGUL LETTER A */|#
  ( #x0ec0 #x3150 ) #|/*                   Hangul_AE ㅐ HANGUL LETTER AE */|#
  ( #x0ec1 #x3151 ) #|/*                   Hangul_YA ㅑ HANGUL LETTER YA */|#
  ( #x0ec2 #x3152 ) #|/*                  Hangul_YAE ㅒ HANGUL LETTER YAE */|#
  ( #x0ec3 #x3153 ) #|/*                   Hangul_EO ㅓ HANGUL LETTER EO */|#
  ( #x0ec4 #x3154 ) #|/*                    Hangul_E ㅔ HANGUL LETTER E */|#
  ( #x0ec5 #x3155 ) #|/*                  Hangul_YEO ㅕ HANGUL LETTER YEO */|#
  ( #x0ec6 #x3156 ) #|/*                   Hangul_YE ㅖ HANGUL LETTER YE */|#
  ( #x0ec7 #x3157 ) #|/*                    Hangul_O ㅗ HANGUL LETTER O */|#
  ( #x0ec8 #x3158 ) #|/*                   Hangul_WA ㅘ HANGUL LETTER WA */|#
  ( #x0ec9 #x3159 ) #|/*                  Hangul_WAE ㅙ HANGUL LETTER WAE */|#
  ( #x0eca #x315a ) #|/*                   Hangul_OE ㅚ HANGUL LETTER OE */|#
  ( #x0ecb #x315b ) #|/*                   Hangul_YO ㅛ HANGUL LETTER YO */|#
  ( #x0ecc #x315c ) #|/*                    Hangul_U ㅜ HANGUL LETTER U */|#
  ( #x0ecd #x315d ) #|/*                  Hangul_WEO ㅝ HANGUL LETTER WEO */|#
  ( #x0ece #x315e ) #|/*                   Hangul_WE ㅞ HANGUL LETTER WE */|#
  ( #x0ecf #x315f ) #|/*                   Hangul_WI ㅟ HANGUL LETTER WI */|#
  ( #x0ed0 #x3160 ) #|/*                   Hangul_YU ㅠ HANGUL LETTER YU */|#
  ( #x0ed1 #x3161 ) #|/*                   Hangul_EU ㅡ HANGUL LETTER EU */|#
  ( #x0ed2 #x3162 ) #|/*                   Hangul_YI ㅢ HANGUL LETTER YI */|#
  ( #x0ed3 #x3163 ) #|/*                    Hangul_I ㅣ HANGUL LETTER I */|#
  ( #x0eef #x316d ) #|/*     Hangul_RieulYeorinHieuh ㅭ HANGUL LETTER RIEUL-YEORINHIEUH */|#
  ( #x0ef0 #x3171 ) #|/*    Hangul_SunkyeongeumMieum ㅱ HANGUL LETTER KAPYEOUNMIEUM */|#
  ( #x0ef1 #x3178 ) #|/*    Hangul_SunkyeongeumPieub ㅸ HANGUL LETTER KAPYEOUNPIEUP */|#
  ( #x0ef2 #x317f ) #|/*              Hangul_PanSios ㅿ HANGUL LETTER PANSIOS */|#
  ( #x0ef3 #x3181 ) #|/*    Hangul_KkogjiDalrinIeung ㆁ HANGUL LETTER YESIEUNG */|#
  ( #x0ef4 #x3184 ) #|/*   Hangul_SunkyeongeumPhieuf ㆄ HANGUL LETTER KAPYEOUNPHIEUPH */|#
  ( #x0ef5 #x3186 ) #|/*          Hangul_YeorinHieuh ㆆ HANGUL LETTER YEORINHIEUH */|#
  ( #x0ef6 #x318d ) #|/*                Hangul_AraeA ㆍ HANGUL LETTER ARAEA */|#
  ( #x0ef7 #x318e ) #|/*               Hangul_AraeAE ㆎ HANGUL LETTER ARAEAE */|#
  ))

(defun gdk-unicode-to-keyval (wc)
  "#|/**
 * gdk_unicode_to_keyval:
 * @wc: a Unicode character
 *
 * Convert from a Unicode character to a key symbol.
 *
 * Returns: the corresponding GDK key symbol, if one exists.
 *   or, if there is no corresponding symbol, wc | #x01000000
 */|#"

  #|/* First check for Latin-1 characters (1:1 mapping) */|#
  (or (if (or (and (>= wc #x0020) (<= wc #x007e))
	      (and (>= wc #x00a0) (<= wc #x00ff)))
	  wc)

      (loop with min = 0
	    with max = (1- (length *gdk-unicode-to-keysym-tab*))
	    #|/* Binary search in table */|#
	    while (>= max min) do
	    (let* ((mid (truncate (+ min max) 2))
		   (elt (elt *gdk-unicode-to-keysym-tab* mid)))
	      (destructuring-bind (keysym ucs) elt
		(cond ((< ucs wc) (setq min (1+ mid)))
		      ((> ucs wc) (setq max (1- mid)))
		      (t #|/* found it */|#
		       (return keysym))))))
      #|/*
      * No matching keysym value found, return Unicode value plus #x01000000
      * (a convention introduced in the UTF-8 work on xterm).
      */|#
      (logior wc  #x01000000)))

