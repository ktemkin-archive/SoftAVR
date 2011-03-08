--************************************************************************************************
-- 8Kx16(8 KB) PM RAM for AVR Core(Xilinx)
-- Version 0.1
-- Designed by Ruslan Lepetenok 
-- Modified by Jack Gassett for use with Papilio
-- Modified 11.06.2009
--************************************************************************************************

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_unsigned.all;

-- For Synplicity Synplify
--library virtexe;
--use	virtexe.components.all; 

-- Aldec
library	unisim;
use unisim.vcomponents.all;

entity XPM8Kx16 is port(
	                  cp2     : in  std_logic;
					  ce      : in  std_logic; 
	                  address : in  std_logic_vector(12 downto 0); 
					  din     : in  std_logic_vector(15 downto 0);		                
					  dout    : out std_logic_vector(15 downto 0);
					  we     : in  std_logic
					  );
end XPM8Kx16;

architecture RTL of XPM8Kx16 is

type   RAMBlDOut_Type is array(2**(address'length-10)-1 downto 0) of  std_logic_vector(dout'range);
signal RAMBlDOut     : RAMBlDOut_Type;

signal WEB     : std_logic_vector(2**(address'length-10)-1 downto 0);
signal gnd      : std_logic;
signal DIP : STD_LOGIC_VECTOR(1 downto 0) := "11";
signal SSR : STD_LOGIC := '0'; -- Don't use the output resets.



begin

gnd <= '0';	

WEB_Dcd:for i in WEB'range generate 
 WEB(i) <= '1' when (we='1' and address(address'high downto 10)=i) else '0';
end generate ;


RAM_Inst:for i in 0 to 2**(address'length-10)-1 generate

RAM_Word:component RAMB16_S18 
generic map (
INIT => X"00000", -- Value of output RAM registers at startup
SRVAL => X"00000", -- Ouput value upon SSR assertion
WRITE_MODE => "WRITE_FIRST", -- WRITE_FIRST, READ_FIRST or NO_CHANGE
-- The following INIT_xx declarations specify the intial contents of the RAM
-- Address 0 to 255
INIT_00 => X"007E940C007E940C007E940C007E940C007E940C007E940C007E940C005B940C",
INIT_01 => X"007E940C007E940C007E940C007E940C007E940C007E940C007E940C007E940C",
INIT_02 => X"007E940C007E940C007E940C007E940C007E940C007E940C007E940C009D940C",
INIT_03 => X"0404040404040030003600000000003200350038000000000031003700000000",
INIT_04 => X"0804020120100804020180402010080402010303030303030202020202020404",
INIT_05 => X"BFDEE0DFEFCFBE1F241100000000000000000604030000010200070000002010",
INIT_06 => X"07B136A2F3C89631920D95D8C004BF0B9503EF0FE0F3E2ECE0B0E6A0E010BFCD",
INIT_07 => X"0000940C0194940C018D940EF7E107B136AB921DC001E0B0E6A2E010BE1BF7C9",
INIT_08 => X"0151940EE0600060918000F5940EE090E080E070E0610151940EE06100609180",
INIT_09 => X"B60F920F921F95080129940EE06100609180950800F5940EE090E080E070E061",
INIT_0A => X"006791300066912093BF93AF939F938F937F936F935F934F933F932F2411920F",
INIT_0B => X"376D5F6A2F671DB11DA196022F822F932FA42FB5006A91700069915000689140",
INIT_0C => X"93A00067939000669380006A93601DB11DA196032F822F932FA42FB5576DF040",
INIT_0D => X"006293801DB11DA19601006591B0006491A00063919000629180006993B00068",
INIT_0E => X"912F913F914F915F916F917F918F919F91AF91BF006593B0006493A000639390",
INIT_0F => X"914094F8B78F2F192F082EF72EE6931F930F92FF92EF9518901F900FBE0F900F",
INIT_10 => X"006891A0006791900066918094F8B72FBF8F0069917000689160006791500066",
INIT_11 => X"90EF90FF910F911FF760071B070A06F916E80BB70BA60B951B84BF2F006991B0",
INIT_12 => X"2FE84F9F57822F932F82E0302F289508BF876081B787BF836084B78394789508",
INIT_13 => X"5AE01FFF0FEEE0F02FE8F0A923882D8095C82FF32FE24F3F58262D9095C82FF9",
INIT_14 => X"938C2B89918C9508938C23899590918CF42923662DB095C896312DA095C84FFF",
INIT_15 => X"2FE84F9F57822F952F842D2095C82FF92FE84F9F558E2F952F84E0502F489508",
INIT_16 => X"B58FF4193023F0512322F10923332D3095C82FF52FE44F5F58462D9095C82FF9",
INIT_17 => X"96312DA095C84FFF59E81FFF0FEEE0F02FE3BD8F7D8FB58FF4193024C004778F",
INIT_18 => X"940E0121940E9508938C2B89918C9508938C23899590918CF42923662DB095C8",
INIT_19 => X"000000000000000000000000000000000000000DCFFF94F8CFFD0080940E0097",
INIT_1A => X"0000000000000000000000000000000000000000000000000000000000000000",
INIT_1B => X"0000000000000000000000000000000000000000000000000000000000000000",
INIT_1C => X"0000000000000000000000000000000000000000000000000000000000000000",
INIT_1D => X"0000000000000000000000000000000000000000000000000000000000000000",
INIT_1E => X"0000000000000000000000000000000000000000000000000000000000000000",
INIT_1F => X"0000000000000000000000000000000000000000000000000000000000000000",
INIT_20 => X"0000000000000000000000000000000000000000000000000000000000000000",
INIT_21 => X"0000000000000000000000000000000000000000000000000000000000000000",
INIT_22 => X"0000000000000000000000000000000000000000000000000000000000000000",
INIT_23 => X"0000000000000000000000000000000000000000000000000000000000000000",
INIT_24 => X"0000000000000000000000000000000000000000000000000000000000000000",
INIT_25 => X"0000000000000000000000000000000000000000000000000000000000000000",
INIT_26 => X"0000000000000000000000000000000000000000000000000000000000000000",
INIT_27 => X"0000000000000000000000000000000000000000000000000000000000000000",
INIT_28 => X"0000000000000000000000000000000000000000000000000000000000000000",
INIT_29 => X"0000000000000000000000000000000000000000000000000000000000000000",
INIT_2A => X"0000000000000000000000000000000000000000000000000000000000000000",
INIT_2B => X"0000000000000000000000000000000000000000000000000000000000000000",
INIT_2C => X"0000000000000000000000000000000000000000000000000000000000000000",
INIT_2D => X"0000000000000000000000000000000000000000000000000000000000000000",
INIT_2E => X"0000000000000000000000000000000000000000000000000000000000000000",
INIT_2F => X"0000000000000000000000000000000000000000000000000000000000000000",
-- Address 768 to 1023
INIT_30 => X"0000000000000000000000000000000000000000000000000000000000000000",
INIT_31 => X"0000000000000000000000000000000000000000000000000000000000000000",
INIT_32 => X"0000000000000000000000000000000000000000000000000000000000000000",
INIT_33 => X"0000000000000000000000000000000000000000000000000000000000000000",
INIT_34 => X"0000000000000000000000000000000000000000000000000000000000000000",
INIT_35 => X"0000000000000000000000000000000000000000000000000000000000000000",
INIT_36 => X"0000000000000000000000000000000000000000000000000000000000000000",
INIT_37 => X"0000000000000000000000000000000000000000000000000000000000000000",
INIT_38 => X"0000000000000000000000000000000000000000000000000000000000000000",
INIT_39 => X"0000000000000000000000000000000000000000000000000000000000000000",
INIT_3A => X"0000000000000000000000000000000000000000000000000000000000000000",
INIT_3B => X"0000000000000000000000000000000000000000000000000000000000000000",
INIT_3C => X"0000000000000000000000000000000000000000000000000000000000000000",
INIT_3D => X"0000000000000000000000000000000000000000000000000000000000000000",
INIT_3E => X"0000000000000000000000000000000000000000000000000000000000000000",
INIT_3F => X"0000000000000000000000000000000000000000000000000000000000000000",
-- The next set of INITP_xx are for the parity bits
-- Address 0 to 255
INITP_00 => X"0000000000000000000000000000000000000000000000000000000000000000",
INITP_01 => X"0000000000000000000000000000000000000000000000000000000000000000",
-- Address 256 to 511
INITP_02 => X"0000000000000000000000000000000000000000000000000000000000000000",
INITP_03 => X"0000000000000000000000000000000000000000000000000000000000000000",
-- Address 512 to 767
INITP_04 => X"0000000000000000000000000000000000000000000000000000000000000000",
INITP_05 => X"0000000000000000000000000000000000000000000000000000000000000000",
-- Address 768 to 1023
INITP_06 => X"0000000000000000000000000000000000000000000000000000000000000000",
INITP_07 => X"0000000000000000000000000000000000000000000000000000000000000000")
port map(
                                      DO   => RAMBlDOut(i)(15 downto 0),
                                      ADDR => address(9 downto 0),
                                      DI   => din(15 downto 0),
												  DIP  => DIP,
                                      EN   => ce,
												  SSR  => SSR,
                                      CLK  => cp2,
                                      WE   => WEB(i)
                                      );
									  
end generate;

-- Output data mux
dout <= RAMBlDOut(CONV_INTEGER(address(address'high downto 10)));



end RTL;
