##
## EPITECH PROJECT, 2018
## evalexpr
## File description:
## Makefile
##

TARGET	=	evalexpr

NAME	=	funEvalExpr

RM		=	rm -f

all:	$(NAME)

$(NAME):
	@stack build
	cp `stack path --local-install-root`/bin/$(TARGET)-exe ./$(NAME)

clean:
	$(RM) $(NAME)

fclean:	clean

re:		fclean all

watch:
	stack build --file-watch $(TARGET)

.PHONY: all clean fclean re
