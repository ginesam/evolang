
all: clean compile

compile:
	ciaoc agent
	ciaoc language_game
	ciaoc language_game_trans

clean: 
	@rm -f agent
	@rm -f language_game
	@rm -f language_game_trans
	@rm -f *.po
	@rm -f *.itf
	@rm -f *.*~
	@rm -f *~
	@rm -f -r */

