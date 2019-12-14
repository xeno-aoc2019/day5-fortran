! Created by  on 2019-12-14.

program read_test
    integer, dimension(1000) :: program
    character(len = 4096) :: line
    integer :: program_length
    integer :: c_index, sep_index, c_opcode, i
    character :: c, COMMA

    integer, parameter :: INST_ADD = 1
    integer, parameter :: INST_MUL = 2
    integer, parameter :: INST_IN = 3
    integer, parameter :: INST_OUT = 4
    integer, parameter :: INST_JT = 5
    integer, parameter :: INST_JF = 6
    integer, parameter :: INST_LT = 7
    integer, parameter :: INST_EQ = 8
    integer, parameter :: INST_HALT = 99

    integer :: inst, r_ip, r_halt, r_input, r_output, pm1, pm2, pm3, arg1, arg2, arg3

    ! COMMA = iachar(',')

    open(1, file = '../input.txt')
    read(1, '(A)') line

    write(*, *) line
    write(*, *) line(1:4)

    strlen = len(line)
    write(*, *) strlen

    c_index = 1
    sep_index = 1
    program_length = 1
    do while (line(c_index - 1:c_index - 1) /= ' ')
        !        if (line == ',') then
        ! write(*, *) 'hello'
        c = line(c_index:c_index)
        !        if (c /= ',') then
        !            write(*, *) c, iachar(c)
        !        end if
        if (c == ',' .or. c == ' ') then
            read(line(sep_index:c_index - 1), *) opcode
            write(*, *) line(sep_index:c_index - 1), opcode
            program(program_length) = opcode
            program_length = program_length + 1
            sep_index = c_index + 1
        end if
        !        end if
        c_index = c_index + 1
    end do

    !--- time to run the machine

    r_ip = 0
    r_halt = 0
    r_input = 5
    r_output = -1

    do while (r_halt == 0)
        inst = MOD (program(r_ip + 1), 100)
        i = (program(r_ip + 1) - inst) / 100
        pm1 = MOD (i, 10)
        pm2 = MOD ((i - pm1) / 10, 10)
        pm3 = MOD ((i - pm2 * 10 - pm1) / 100, 10)
        write(*, *) "INST ", inst, pm1, pm2, pm3
        if (inst == INST_ADD) then
            arg1 = program(r_ip + 2)
            if (pm1 == 0) then
                arg1 = program(arg1 + 1)
            end if
            arg2 = program(r_ip + 3)
            if (pm2 == 0) then
                arg2 = program(arg2 + 1)
            end if
            arg3 = program(r_ip + 4)
            write(*, *) "ADD ", arg3, arg1, arg2
            program(arg3 + 1) = arg1 + arg2
            r_ip = r_ip + 4
        elseif (inst == INST_MUL) then
            arg1 = program(r_ip + 2)
            if (pm1 == 0) then
                arg1 = program(arg1 + 1)
            end if
            arg2 = program(r_ip + 3)
            if (pm2 == 0) then
                arg2 = program(arg2 + 1)
            end if
            arg3 = program(r_ip + 4)
            write(*, *) "MUL ", arg3, arg1, arg2
            program(arg3 + 1) = arg1 * arg2
            r_ip = r_ip + 4
        elseif (inst == INST_MUL) then
            write(*, *) "NOT IMPLEMENTED", inst
            r_halt = 1
        elseif (inst == INST_IN) then
            arg1 = program(r_ip + 2)
            program(arg1 + 1) = r_input
            r_ip = r_ip + 2
        elseif (inst == INST_OUT) then
            arg1 = program(r_ip + 2)
            r_output = program(arg1 + 1)
            r_ip = r_ip + 2
        elseif (inst == INST_JT) then
            arg1 = program(r_ip + 2)
            if (pm1 == 0) then
                arg1 = program(arg1 + 1)
            end if
            arg2 = program(r_ip + 3)
            if (pm2 == 0) then
                arg2 = program(arg2 + 1)
            end if
            if (arg1 /= 0) then
                r_ip = arg2
            else
                r_ip = r_ip + 3
            end if
            write(*, *) "JT ", arg1, arg2
        elseif (inst == INST_JF) then
            arg1 = program(r_ip + 2)
            if (pm1 == 0) then
                arg1 = program(arg1 + 1)
            end if
            arg2 = program(r_ip + 3)
            if (pm2 == 0) then
                arg2 = program(arg2 + 1)
            end if
            if (arg1 == 0) then
                r_ip = arg2
            else
                r_ip = r_ip + 3
            end if
            write(*, *) "JF ", arg1, arg2
        elseif (inst == INST_LT) then
            arg1 = program(r_ip + 2)
            if (pm1 == 0) then
                arg1 = program(arg1 + 1)
            end if
            arg2 = program(r_ip + 3)
            if (pm2 == 0) then
                arg2 = program(arg2 + 1)
            end if
            arg3 = program(r_ip + 4)
            write(*, *) "LT ", arg3, arg1, arg2
            if (arg1 < arg2) then
                program(arg3 + 1) = 1
            else
                program(arg3 + 1) = 0
            end if
            r_ip = r_ip + 4
        elseif (inst == INST_EQ) then
            arg1 = program(r_ip + 2)
            if (pm1 == 0) then
                arg1 = program(arg1 + 1)
            end if
            arg2 = program(r_ip + 3)
            if (pm2 == 0) then
                arg2 = program(arg2 + 1)
            end if
            arg3 = program(r_ip + 4)
            write(*, *) "EQ ", arg3, arg1, arg2
            if (arg1 == arg2) then
                program(arg3 + 1) = 1
            else
                program(arg3 + 1) = 0
            end if
            r_ip = r_ip + 4
        elseif (inst == INST_HALT) then
            write(*, *) "HALT"
            r_halt = 1
        else
            r_halt = 1
            write(*, *) 'ERROR UNKNOWN INSTRUCTION', inst
        end if
    end do

    write(*, *) "OUTPUT: ", r_output

end program read_test