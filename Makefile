##
## EPITECH PROJECT, 2017
## Makefile
## File description:
## makefile of the source file
##


GREEN	=	\e[1;32m

WHITE	=	\e[0;m

ORANGE	=	\e[1;33m

RED		=	\e[91m

NAME	=	imageCompressor

CC	=	stack

all:	$(NAME)

$(NAME):
	@$(CC) build
	@printf "$(GREEN)<$(NAME)> compile sources.$(WHITE)\n"
	@cp `stack path --local-install-root`/bin/$(NAME)-exe ./$(NAME)
	@printf "$(GREEN)<$(NAME)> copy $(NAME) at root of Project$(WHITE)\n"

clean:
	@$(CC) clean
	@printf "$(RED)<$(NAME)> clean.$(WHITE)\n"

fclean: clean
	@rm -rf ./$(NAME)
	@printf "$(RED)<$(NAME)> fclean.$(WHITE)\n"

re:	fclean all

.PHONY: fclean clean all re debug
