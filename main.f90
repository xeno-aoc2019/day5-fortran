program day5_fortran
    implicit none
    integer :: INPUT_FD
    integer :: io_status
    integer :: read_status
    integer :: read_value
    logical :: file_exists
    integer :: read_length
    integer :: c
    integer, dimension(:), allocatable :: bytes
    integer, dimension(:), allocatable :: bytes_back
    integer bytes_size
    integer bytes_end
    integer, dimension(5) :: intbytes
    integer, external :: read_characters
    integer :: c1,c2,c3,c4

    INPUT_FD = 9
    io_status = 42
    read_value = 42
    file_exists = 0
    read_status = 42
    read_length = 15

    inquire(file = "input.txt", exist = file_exists)
    open(unit = INPUT_FD, file = "input.txt", iostat = io_status, action = 'read', status = 'old', access = 'stream')

    write(*, *) "exists ", file_exists

    read(INPUT_FD) read_value

    write(*, *) "Hello, World!"
    write(*, *) io_status
    write(*, *) read_status
    write(*, *) read_value
    allocate(bytes(8))

    c1 = intbytes(0)
    c2 = intbytes(1)
    c3 = intbytes(2)
    c4 = intbytes(3)

    intbytes = read_characters(read_value)
    write(*, *) "Bytes: "
    write(*, *) c1
    write(*, *) c2
    write(*, *) c3
    write(*, *) c4
    write(*, *)
    c = mod(read_value, 256)
    bytes(1) = c
    read_value = read_value - c
    read_value = read_value / 256
    write(*, *) c
    c = mod(read_value, 256)
    write(*, *) c

end program

subroutine shift_left_8(val)
    implicit none
    integer, intent(inout) :: val
    integer :: i
    i = mod(val, 256)
    val = val - i
    val = val / 256
end subroutine shift_left_8

function last_ubyte(val) result(b)
    integer, intent(in) :: val
    integer, intent(out) :: b
    b = mod(val, 256)
end function

function read_characters(i) result(cd)
    integer, intent(in) :: i
    integer, dimension(5), intent(out) :: cd

    cd(0) = last_ubyte(i)
    call shift_left_8(i)
    cd(1) = last_ubyte(i)
    call shift_left_8(i)
    cd(2) = last_ubyte(i)
    call shift_left_8(i)
    cd(3) = last_byte(i)
end function read_characters
