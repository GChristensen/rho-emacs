################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
CPP_SRCS += \
../CmdLine.cpp \
../StrUtils.cpp \
../launcher.cpp \
../select.cpp 

CPP_SRCS_T += \
../CmdLine.cpp \
../StrUtils.cpp \
../launcher.cpp

OBJS += \
./CmdLine.o \
./StrUtils.o \
./launcher.o \
./select.o 

OBJS_T += \
./CmdLine.o \
./StrUtils.o \
./launcher.o

CPP_DEPS += \
./CmdLine.d \
./StrUtils.d \
./launcher.d \
./select.d 

CPP_DEPS_T += \
./CmdLine.d \
./StrUtils.d \
./launcher.d

# Each subdirectory must supply rules for building sources it contributes
%.o: ../%.cpp
	@echo 'Building file: $<'
	@echo 'Invoking: GCC C++ Compiler'
	g++ -DUNICODE -D_UNICODE -D_X86_ -m32 -O3 -Wall -c -fmessage-length=0 -MMD -MP -MF"$(@:%.o=%.d)" -MT"$(@:%.o=%.d)" -o"$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '


