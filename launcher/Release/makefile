################################################################################
# Automatically-generated file. Do not edit!
################################################################################

-include ../makefile.init

RM := rm -rf

CXXFLAGS ?= -static-libgcc -static-libstdc++
LDFLAGS ?= -static-libgcc -static-libstdc++

# All of the sources participating in the build are defined here
-include sources.mk
-include subdir.mk
-include objects.mk

ifneq ($(MAKECMDGOALS),clean)
ifneq ($(strip $(C++_DEPS)),)
-include $(C++_DEPS)
endif
ifneq ($(strip $(C_DEPS)),)
-include $(C_DEPS)
endif
ifneq ($(strip $(CC_DEPS)),)
-include $(CC_DEPS)
endif
ifneq ($(strip $(CPP_DEPS)),)
-include $(CPP_DEPS)
endif
ifneq ($(strip $(CXX_DEPS)),)
-include $(CXX_DEPS)
endif
ifneq ($(strip $(C_UPPER_DEPS)),)
-include $(C_UPPER_DEPS)
endif
endif

-include ../makefile.defs

# Add inputs and outputs from these tool invocations to the build variables 

# All Target
.PHONY: all
all: ./resources.o ./rho.exe

# Tool invocations
./rho.exe: $(OBJS) $(USER_OBJS)
	@echo 'Building target: $@'
	@echo 'Invoking: MinGW C++ Linker'
	g++ -mwindows -o"./rho.exe" -static-libgcc -static-libstdc++ -static -m32 $(OBJS) $(USER_OBJS) $(LIBS) -lmsimg32
	@echo 'Finished building target: $@'
	@echo ' '

./resources.o: ../resources.rc
	windres -i ../resources.rc --input-format=rc -o ./resources.o

term: ./resources.o ./rhoc.exe

# Tool invocations
./rhoc.exe: $(OBJS_T) $(USER_OBJS)
	@echo 'Building target: $@'
	@echo 'Invoking: MinGW C++ Linker'
	g++ -DTERMINAL="" -mconsole -o"./rhoc.exe" -static-libgcc -static-libstdc++ -static $(OBJS_T) $(USER_OBJS) $(LIBS) 
	@echo 'Finished building target: $@'
	@echo ' '


# Other Targets
clean:
	-$(RM) $(C++_DEPS)$(OBJS)$(C_DEPS)$(CC_DEPS)$(CPP_DEPS)$(EXECUTABLES)$(CXX_DEPS)$(C_UPPER_DEPS)$(USER_OBJS)
	-$(RM) ./rho.exe
	-@echo ' '

.PHONY: all clean dependents
.SECONDARY:

-include ../makefile.targets
