LIBRARY ieee;
USE ieee.std_logic_1164.all;

ENTITY vhdl_game_vga IS

	GENERIC(
		h_pulse 	:	INTEGER   := 96;    	--horiztonal sync pulse width in pixels
		h_bp	 	:	INTEGER   := 48;		--horiztonal back porch width in pixels
		h_pixels	:	INTEGER   := 640;		--horiztonal display width in pixels
		h_fp	 	:	INTEGER   := 16;		--horiztonal front porch width in pixels
		h_pol		:	STD_LOGIC := '0';		--horizontal sync pulse polarity (1 = positive, 0 = negative)
		v_pulse 	:	INTEGER   := 2;			--vertical sync pulse width in rows
		v_bp	 	:	INTEGER   := 33;			--vertical back porch width in rows
		v_pixels	:	INTEGER   := 480;		--vertical display width in rows
		v_fp	 	:	INTEGER   := 10;			--vertical front porch width in rows
		v_pol		:	STD_LOGIC := '1');	--vertical sync pulse polarity (1 = positive, 0 = negative)
	PORT(
		clk      	:	IN		STD_LOGIC;	--pixel clock at frequency of VGA mode being used
		reset_n		:	IN		STD_LOGIC;	--active low asycnchronous reset
		button_j    :  in    std_LOGIC;
		h_sync		:	OUT	STD_LOGIC;	--horiztonal sync pulse
		v_sync		:	OUT	STD_LOGIC;	--vertical sync pulse
		n_blank		:	OUT	STD_LOGIC;	--direct blacking output to DAC
		n_sync		:	OUT	STD_LOGIC; --sync-on-green output to DAC
		red			:	OUT	STD_LOGIC_VECTOR(7 DOWNTO 0) := (OTHERS => '0');  --red magnitude output to DAC
		green			:	OUT	STD_LOGIC_VECTOR(7 DOWNTO 0) := (OTHERS => '0');  --green magnitude output to DAC
		blue			:	OUT	STD_LOGIC_VECTOR(7 DOWNTO 0) := (OTHERS => '0'); --blue magnitude output to DAC
		clk_vga     :  out   std_logic; --vga clk 25MHz
		rw, rs, e   :  OUT   STD_LOGIC; --read/write, setup/data, and enable for lcd
		lcd_data    :  OUT   STD_LOGIC_VECTOR(7 DOWNTO 0); --data signals for lcd
		lcd_on      :  OUT   std_logic; --LCD Power ON/OFF
		lcd_blon    :  OUT   std_logic); --LCD Back Light ON/OFF
END vhdl_game_vga;

ARCHITECTURE behavior OF vhdl_game_vga IS
    --state machine for lcd 
	TYPE CONTROL IS(power_up, initialize, ready, send);
	SIGNAL state : CONTROL;
	CONSTANT freq : INTEGER := 50; --system clock frequency in MHz
	signal RESET_M: std_LOGIC;     --lcd reset
	signal lcd_enable:std_LOGIC;
	signal lcd_bus:STD_LOGIC_VECTOR(9 DOWNTO 0);
	signal busy, lcd_busy : std_LOGIC:='1';
	signal lcd_clk : std_LOGIC;signal disp_ena   :std_LOGIC;
	--vga sync
	signal column     :integer;
	signal row        :integer;
	signal pixel_clk  :  std_logic;
	CONSTANT	h_period	:	INTEGER := h_pulse + h_bp + h_pixels + h_fp;  --total number of pixel clocks in a row
	CONSTANT	v_period	:	INTEGER := v_pulse + v_bp + v_pixels + v_fp;  --total number of rows in column
--
--
--
--gameover
constant gameover_l:integer :=0;--the distance between the gameover screen and left side of screen
constant gameover_t:integer :=0;--the distance between the gameover screen and top side of screen
constant gameover_k:integer :=460;--gameover thickness
signal lose_on, lose_on_next :std_logic:='0';
signal over_on, over_on_next :std_logic:='0';
signal gameover_on   :std_logic;
signal red_gameover  :std_logic_vector(7 downto 0); 
signal blue_gameover :std_logic_vector(7 downto 0); 
signal green_gameover:std_logic_vector(7 downto 0); 

--character
signal character_t,character_t_next:integer :=420; --the distance between character and left side of screen
constant character_l:integer :=200;--the distance between character and top side of screen
constant character_k:integer :=20;--character thickness
constant character_w:integer:=20;--character width
constant character_v:integer:=10;--horizontal speed of the character
signal character_on   :std_logic;
signal red_character  :std_logic_vector(7 downto 0); 
signal green_character:std_logic_vector(7 downto 0); 
signal blue_character :std_logic_vector(7 downto 0); 

--obstacle
signal obstacle_l,obstacle_og,obstacle_l_next:integer :=600;--the distance between obstacle and left side of screen
constant obstacle_t:integer :=420; --the distance between obstacle and top side of screen
constant obstacle_h:integer :=20;--obstacle Height
constant obstacle_u:integer :=20;--obstacle width
constant obstacle_v:integer:=3;-- horizontal speeds of the obstacle 
signal obstacle_on :std_logic;
signal green_obstacle:std_logic_vector(7 downto 0); 
signal blue_obstacle :std_logic_vector(7 downto 0); 
signal red_obstacle  :std_logic_vector(7 downto 0); 

--refreshing(1/60) of 640*480 screen 
signal refresh_reg,refresh_next:integer;
constant refresh_constant:integer:=833333;
signal refresh_tick:std_logic;

--obstacle animation
signal obstacle_v_reg,obstacle_v_next:integer:=3;--variable of the horizontal speed

--x,y pixel cursor
signal x,y:integer range 0 to 650;

--mux
signal mux:std_logic_vector(3 downto 0);
	
--color refresh
signal red_reg,red_next     :std_logic_vector(7 downto 0);
signal green_reg,green_next :std_logic_vector(7 downto 0);	
signal blue_reg,blue_next   :std_logic_vector(7 downto 0);
	
	BEGIN
------------------LCD DISPLAY-----------
----------
--Part one
----------
 lcd_on <= '1'; --LCD Power ON
 lcd_blon<='1'; --LCD Back Light ON

 PROCESS(clk)
VARIABLE clk_count : INTEGER := 0; --event counter for timing
	BEGIN
	IF(clk'EVENT and clk = '1') THEN

		CASE state IS

 --wait 50 ms to ensure Vdd has risen and required LCD wait is met
WHEN power_up =>
busy <= '1';
 IF(clk_count < (50000 * freq)) THEN --wait 50 ms
 clk_count := clk_count + 1;
 state <= power_up;
 ELSE --power-up complete
 clk_count := 0;
 rs <= '0';
 rw <= '0';
 lcd_data <= "00110000"; -- Function Set: 1-line mode,display off lcd_data <= "00110000";
 state <= initialize;
 END IF;

 --cycle through initialization sequence
 WHEN initialize =>
 busy <= '1';
 clk_count := clk_count + 1;
 IF(clk_count < (10 * freq)) THEN --function set
 -- lcd_data <= "00111100"; --2-line mode, display on
 lcd_data <= "00110100"; --1-line mode, display on
 --lcd_data <= "00110000"; --1-line mdoe, display off
 --lcd_data <= "00111000"; --2-line mode, display off
 e <= '1';
 state <= initialize;
 ELSIF(clk_count < (60 * freq)) THEN --wait 50 us
 lcd_data <= "00000000";
 e <= '0';
 state <= initialize;
 ELSIF(clk_count < (70 * freq)) THEN --display on/off control
 --lcd_data <= "00001100"; --display on, cursor off, blink off
 lcd_data <= "00001101"; --display on, cursor off, blink on
 --lcd_data <= "00001110"; --display on, cursor on, blink off
 --lcd_data <= "00001111"; --display on, cursor on, blink on
 --lcd_data <= "00001000"; --display off, cursor off, blink off
 --lcd_data <= "00001001"; --display off, cursor off, blink on
 --lcd_data <= "00001010"; --display off, cursor on, blink off
 --lcd_data <= "00001011"; --display off, cursor on, blink on
 e <= '1';
 state <= initialize;
 ELSIF(clk_count < (120 * freq)) THEN --wait 50 us
 lcd_data <= "00000000";
 e <= '0';
 state <= initialize;
 ELSIF(clk_count < (130 * freq)) THEN --display clear
 lcd_data <= "00000001";
e <= '1';
 state <= initialize;
 ELSIF(clk_count < (2130 * freq)) THEN --wait 2 ms
 lcd_data <= "00000000";
e <= '0';
 state <= initialize;
 ELSIF(clk_count < (2140 * freq)) THEN --entry mode set
 lcd_data <= "00000110"; --increment mode, entire shift off
 --lcd_data <= "00000111"; --increment mode, entire shift on
 --lcd_data <= "00000100"; --decrement mode, entire shift off
 --lcd_data <= "00000101"; --decrement mode, entire shift on
 e <= '1';
 state <= initialize;
 ELSIF(clk_count < (2200 * freq)) THEN --wait 60 us
 lcd_data <= "00000000";
 e <= '0';
 state <= initialize;
 ELSE --initialization complete
 clk_count := 0;
 busy <= '0';
 state <= ready;
 END IF;

 --wait for the enable signal and then latch in the instruction
WHEN ready =>
 IF(lcd_enable = '1' ) THEN
 busy <= '1';
 rs <= lcd_bus(9);
 --rs<= lcd_rs;
 rw <= lcd_bus(8);
 --rw <= lcd_rw;
 lcd_data <= lcd_bus(7 DOWNTO 0);
 --lcd_data <= lcd_bus;
 clk_count := 0;
 state <= send;
 ELSE
 busy <= '0';
 rs <= '0';
 rw <= '0';
 lcd_data <= "00000000";
 clk_count := 0;
 state <= ready;
 END IF;

 --send instruction to lcd
 WHEN send =>
 busy <= '1';
 IF(clk_count < (50 * freq)) THEN --do not exit for 50us
 busy <= '1';
 IF(clk_count < freq) THEN --negative enable 
 e <= '0';
 ELSIF(clk_count < (14 * freq)) THEN --positive enable half-cycle
 e <= '1';
 ELSIF(clk_count < (27 * freq)) THEN --negative enable half-cycle
 e <= '0';
 END IF;
 clk_count := clk_count + 1;
 state <= send;
 ELSE
 clk_count := 0;
 state <= ready;
 END IF;

 END CASE;

 --reset
IF(RESET_M = '0') THEN
 state <= power_up;
END IF;

END IF;
LCD_busy<=BUSy;
END PROCESS;
------------------
--Part Two
------------------
PROCESS(clk)
VARIABLE char : INTEGER RANGE 0 TO 10 := 0;
BEGIN
IF(clk'EVENT AND clk = '1' AND gameover_on ='1') THEN
IF(lcd_busy = '0' AND lcd_enable = '0') THEN
lcd_enable <= '1';
IF(char < 12) THEN
char := char + 1;
 END IF;
CASE char IS
WHEN 1 => lcd_bus <= "1001000111"; --"G"
WHEN 2 => lcd_bus <= "1001000001"; --"A"
WHEN 3 => lcd_bus <= "1001001101"; --"M"
WHEN 4 => lcd_bus <= "1001000101"; --"E"
WHEN 5 => lcd_bus <= "1000100000"; --" "
WHEN 6 => lcd_bus <= "1001001111"; --"O"
WHEN 7 => lcd_bus <= "1001010110"; --"V"
WHEN 8 => lcd_bus <= "1001000101"; --"E"
WHEN 9 => lcd_bus <= "1001010010"; --"R"

WHEN OTHERS => lcd_enable <= '0';
END CASE;
ELSE
lcd_enable <= '0';
END IF;
END IF;
END PROCESS;

RESET_M <= '1';
lcd_clk <= clk;
	
---------------LCD DISPLAY END	-------------------------------
----------------------------------------------------------------
---------------VGA SYNC ----------------------------------------	
process(clk)  --25MHz clk for vga refresh
begin
       if(clk'event and clk = '1')then
              pixel_clk <= not pixel_clk;
       end if;
		 clk_vga<=pixel_clk;
end process;
	--vga sync start
	n_blank <= '1';  --no direct blanking
	n_sync  <= '0';   --no sync on green
	
	PROCESS(pixel_clk, reset_n)
		VARIABLE h_count	:	INTEGER RANGE 0 TO h_period - 1 := 0;  --horizontal counter (counts the columns)
		VARIABLE v_count	:	INTEGER RANGE 0 TO v_period - 1 := 0;  --vertical counter (counts the rows)
	BEGIN
	
		IF(reset_n = '0') THEN		--reset asserted
			h_count := 0;				--reset horizontal counter
			v_count := 0;				--reset vertical counter
			h_sync <= NOT h_pol;		--deassert horizontal sync
			v_sync <= NOT v_pol;		--deassert vertical sync
			disp_ena <= '0';			--disable display
			column <= 0;				--reset column pixel coordinate
			row <= 0;					--reset row pixel coordinate
			
		ELSIF(pixel_clk'EVENT AND pixel_clk = '1') THEN

			--counters
			IF(h_count < h_period - 1) THEN		--horizontal counter (pixels)
				h_count := h_count + 1;
			ELSE
				h_count := 0;
				IF(v_count < v_period - 1) THEN	--veritcal counter (rows)
					v_count := v_count + 1;
				ELSE
					v_count := 0;
				END IF;
			END IF;

			--horizontal sync signal
			IF(h_count < h_pixels + h_fp OR h_count > h_pixels + h_fp + h_pulse) THEN
				h_sync <= NOT h_pol;		--deassert horiztonal sync pulse
			ELSE
				h_sync <= h_pol;			--assert horiztonal sync pulse
			END IF;
			
			--vertical sync signal
			IF(v_count < v_pixels + v_fp OR v_count > v_pixels + v_fp + v_pulse) THEN
				v_sync <= NOT v_pol;		--deassert vertical sync pulse
			ELSE
				v_sync <= v_pol;			--assert vertical sync pulse
			END IF;
			
			--set pixel coordinates
			IF(h_count < h_pixels) THEN  	--horiztonal display time
				column <= h_count;			--set horiztonal pixel coordinate
			END IF;
			IF(v_count < v_pixels) THEN	--vertical display time
				row <= v_count;				--set vertical pixel coordinate
			END IF;

			--set display enable output
			IF(h_count < h_pixels AND v_count < v_pixels) THEN  	--display time
				disp_ena <= '1';											 	--enable display
			ELSE																	--blanking time
				disp_ena <= '0';												--disable display
			END IF;

		END IF;
	END PROCESS;
------------------------------VGA SYNC END--------------------------
--------------------------------------------------------------------
------------------------------IMAGE GENERATION----------------------
--horizontal & vertical  cursor
x <=column;
y <=row;

--refreshing
process(clk)
begin
     if clk'event and clk='1' then
          refresh_reg<=refresh_next; 
     end if;
end process;
refresh_next<= 0 when refresh_reg= refresh_constant else
refresh_reg+1;
refresh_tick<= '1' when refresh_reg = 0 else
                           '0';
--register data
process(clk)
begin
     if clk'event and clk='1' then
         obstacle_v_reg<=obstacle_v_next;
         character_t<=character_t_next;
         obstacle_l<=obstacle_l_next;
			over_on<=over_on_next;
--			lose_on<=lose_on_next;
      end if;
end process;

--animation
process(refresh_tick,obstacle_l,obstacle_v_reg,character_t,button_j,lose_on)
begin
     character_t_next<=character_t;
	  obstacle_l_next <=obstacle_l;
     obstacle_v_next<=obstacle_v_reg;
	  
     if refresh_tick = '1' then 
			if obstacle_l >10 then 
				obstacle_l_next<=obstacle_l-obstacle_v;
			else
				obstacle_l_next<=600;
			end if;
	 	
			if button_j='0' and character_t > character_v then 
				character_t_next<=character_t - character_v;
			else
				if character_t > 420 then
					character_t_next<=character_t;
				else
					character_t_next<=character_t + character_v;
				end if;
			end if;
	  end if;
		
		if obstacle_l > 200 or  obstacle_l< 160  or character_t+20<obstacle_t then
			lose_on <= '0';
			over_on_next<=over_on;
		else 
			lose_on <= '1';
			over_on_next <= lose_on;
		end if; 	 
		
		if x > gameover_l and x < (640-gameover_l) and y> gameover_t and y < (gameover_t+ gameover_k) AND (over_on='1' ) then
			gameover_on<='1';
		elsE
			gameover_on<='0';
		end if;
		
		if x > character_l and x < (character_l+character_w) and y> character_t-character_v and y < (character_t+ character_k)-character_v then
			character_on<='1';
		elsE
			character_on<='0';
		end if;

		if x > obstacle_l and x < (obstacle_l+obstacle_u) and y> obstacle_t and y < (obstacle_t+ obstacle_h) then
			obstacle_on<='1';
		elsE
			obstacle_on<='0';
		end if;
end process;


--gameover object
red_gameover  <=(OTHERS => '0');  --Black
blue_gameover <=(OTHERS => '0');  --Black
green_gameover<=(OTHERS => '0');  --Black
--character object
red_character  <=(OTHERS => '0');  
blue_character <=(OTHERS => '1');  
green_character<=(OTHERS => '0');  --blue

--obstacle object
red_obstacle  <=(OTHERS => '0');  
blue_obstacle <=(OTHERS => '0');  
green_obstacle<=(OTHERS => '1');  --Green

--color refresh	
process(clk)
begin
     if clk'event and clk='1' then
         red_reg<=red_next;
			green_reg<=green_next;
			blue_reg<=blue_next;
     end if;
end process;

	--mux
	mux<=disp_ena & gameover_on & character_on &obstacle_on; --16 to 1 mux select line      
	with mux select
		red_next <= (OTHERS => '1')            when "1000", --red background 
		            red_gameover               when "1100",
		            red_gameover               when "1101",
		            red_character              when "1010",
		            red_character              when "1011",
		            red_obstacle               when "1001",
	               (OTHERS => '0')            when others;
	
	with mux select					
		green_next <= (OTHERS => '0')          when "1000",  
		            green_gameover    			When "1100",
		            green_gameover             when "1101",
		            green_character            when "1010",
		            green_character            when "1011",
		            green_obstacle             when "1001",
	               (OTHERS => '0')            when others;
	
	with mux select
		blue_next <= (OTHERS => '0')           when "1000",  
		            blue_gameover              when "1100",
		            blue_gameover              when "1101",
		            blue_character             when "1010",
		            blue_character             when "1011",
		            blue_obstacle              when "1001",
	               (OTHERS => '0')            when others;
	--color output
	red  <=red_reg;
	blue <=blue_reg;
	green<=green_reg;
------------IMAGE GENERATION END--------------------
END behavior;