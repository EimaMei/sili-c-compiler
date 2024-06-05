CC = clang
OUTPUT = build
NAME = test
OS = LINUX

SRC-DIR = src
DEPS-DIR = $(SRC-DIR)/deps
INCLUDE = -I"include" -I"." -I"deps" -I"src"
FLAGS = -std=c99 -Wall -Wextra -Wpedantic
MAC_GEN_APP = false


ifeq ($(OS),WINDOWS)
	EXE = $(OUTPUT)/$(NAME).exe
	LIBS =
	DEPS-SRC = $(notdir $(wildcard $(DEPS-DIR)/*.c))

else ifeq ($(OS),MAC)
	EXE = $(OUTPUT)/$(NAME)
	LIBS =
	DEPS-SRC = $(notdir $(wildcard $(DEPS-DIR)/*.c /$(DEPS-DIR)/mac/*.c))

else
	EXE = $(OUTPUT)/$(NAME)
	LIBS =
	DEPS-SRC = $(notdir $(wildcard $(DEPS-DIR)/*.c))
endif

# do not edit this
SRC-FILES = $(notdir $(wildcard $(SRC-DIR)/*.c))
SRC-OBJ = $(addprefix $(OUTPUT)/, $(SRC-FILES:.c=.o))
DEPS-OBJ = $(addprefix $(OUTPUT)/, $(DEPS-SRC:.c=.o))

# 'make'
all: $(OUTPUT) $(EXE) run

# Run the exe.
run: $(EXE)
ifeq ($(MAC_GEN_APP), true)
	make generateApp
	open $(OUTPUT)/$(NAME).app
else
	./$(EXE)
endif

# Clean the 'build' folder.
clean:
	rm $(OUTPUT)/**

$(EXE): $(DEPS-OBJ) $(SRC-OBJ)
	$(CC) $(FLAGS) $^ $(LIBS) -o $@
ifeq ($(OS),MAC)
#make generateApp
endif

$(OUTPUT)/%.o: $(SRC-DIR)/%.c
	$(CC) $(FLAGS) $(INCLUDE) -c $^ -o $(OUTPUT)/$(notdir $@)


$(OUTPUT)/%.o: %.h
	$(CC) $(FLAGS) $(INCLUDE) -c $(DEPS-DIR)/$(basename $(notdir $^)).c -o $(OUTPUT)/$(notdir $@)

$(OUTPUT)/%.o: include/%.h
	$(CC) $(FLAGS) $(INCLUDE) -c $(DEPS-DIR)/$(basename $(notdir $^)).c -o $(OUTPUT)/$(notdir $@)

$(OUTPUT)/%.o: include/sili/%.h
	$(CC) $(FLAGS) $(INCLUDE) -c $(DEPS-DIR)/$(basename $(notdir $^)).c -o $(OUTPUT)/$(notdir $@)

$(OUTPUT)/%.o: include/stb/%.h
	$(CC) $(FLAGS) $(INCLUDE) -c $(DEPS-DIR)/$(basename $(notdir $^)).c -o $(OUTPUT)/$(notdir $@)

# If 'build' doesn't exist, create it
$(OUTPUT):
	mkdir $(OUTPUT)
