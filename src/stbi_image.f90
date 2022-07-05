module stb_image_mod

    use, intrinsic :: iso_c_binding
    
    implicit none

    private :: c_stbi_failure_reason
    ! public :: stbi_loadf, stbi_load, stbi_info

    interface 
        ! STBIDEF void stbi_set_flip_vertically_on_load(int flag_true_if_should_flip);
        subroutine c_stbi_set_flip_vertically_on_load(flag) bind(c, name="stbi_set_flip_vertically_on_load")
            import :: c_int
            integer(c_int), value, intent(IN) :: flag
        end subroutine c_stbi_set_flip_vertically_on_load
    end interface

    interface
        ! STBIDEF const char *stbi_failure_reason  (void);
        function c_stbi_failure_reason() bind(c, name="stbi_failure_reason")
            import :: c_ptr
            type(c_ptr) :: c_stbi_failure_reason
        end function c_stbi_failure_reason
    end interface

    interface
        ! STBIDEF int stbi_is_hdr(char const *filename)
        function c_stbi_is_hdr(filename) bind(c, name="stbi_is_hdr")
        import :: c_char, c_int
        integer(kind=c_int) :: c_stbi_is_hdr
        character(kind=c_char), intent(IN) :: filename(*)
        end function c_stbi_is_hdr
    end interface

    interface
        ! STBIDEF void stbi_hdr_to_ldr_gamma(float gamma);
        subroutine c_stbi_hdr_to_ldr_gamma(gamma) bind(c, name="stbi_hdr_to_ldr_gamma")
            import :: c_float
            real(kind=c_float), intent(IN) :: gamma
        end subroutine c_stbi_hdr_to_ldr_gamma
    end interface

    interface
        ! STBIDEF void stbi_hdr_to_ldr_scale(float scale);
        subroutine c_stbi_hdr_to_ldr_scale(scale) bind(c, name="stbi_hdr_to_ldr_scale")
            import :: c_float
            real(kind=c_float), intent(IN) :: scale
        end subroutine c_stbi_hdr_to_ldr_scale
    end interface

    interface
        ! STBIDEF void stbi_hdr_to_ldr_gamma(float gamma);
        subroutine c_stbi_ldr_to_hdr_gamma(gamma) bind(c, name="stbi_ldr_to_hdr_gamma")
            import :: c_float
            real(kind=c_float), intent(IN) :: gamma
        end subroutine c_stbi_ldr_to_hdr_gamma
    end interface

    interface
        ! STBIDEF void stbi_hdr_to_ldr_scale(float scale);
        subroutine c_stbi_ldr_to_hdr_scale(scale) bind(c, name="stbi_ldr_to_hdr_scale")
            import :: c_float
            real(kind=c_float), intent(IN) :: scale
        end subroutine c_stbi_ldr_to_hdr_scale
    end interface

    interface
        ! STBIDEF stbi_uc *stbi_load(char const *filename, int *x, int *y, int *channels_in_file, int desired_channels)
        function c_stbi_load(filename, x, y, n_channels, req_n_channels) bind(c, name="stbi_load")
            import :: c_char, c_int, c_ptr
            character(kind=c_char), intent(IN) :: filename(*)
            type(c_ptr) :: c_stbi_load
            integer(c_int), intent(OUT) :: x, y, n_channels
            integer(c_int), value :: req_n_channels
        end function
    end interface

    interface
        ! STBIDEF float *stbi_loadf(char const *filename, int *x, int *y, int *channels_in_file, int desired_channels);
        function c_stbi_loadf(filename, x, y, n_channels, req_n_channels) bind(c, name="stbi_loadf")
        import :: c_char, c_int, c_ptr
        character(kind=c_char), dimension(*), intent(IN) :: filename
        integer(c_int) :: x, y, n_channels
        integer(c_int), value :: req_n_channels
        type(c_ptr) :: c_stbi_loadf
        end function
    end interface
    
    interface
        ! stbi_info(filename, &x, &y, &n)
        function stbi_info(filename, x, y, n) bind(c, name="stbi_info")
        import :: c_char, c_int
        character(kind=c_char), dimension(*), intent(IN) :: filename
        integer(c_int) :: stbi_info
        integer(c_int) :: x, y, n
        end function
    end interface

    contains

    function stbi_failure_reason() result(res)

        implicit none

        character(kind=c_char), dimension(:), pointer :: char_array_pointer => null()
        character(len=:), allocatable :: res
        character(len=1) :: char
        integer :: i

        call c_f_pointer(c_stbi_failure_reason(), char_array_pointer, [256])

        res = repeat(" ", 256)
        do i = 1, 256
            char = char_array_pointer(i)
            if(char == C_Null_char)exit
            res(i:i) = char
        end do
        res = trim(res)

    end function stbi_failure_reason

    function stbi_load(filename, x, y, n_channels, req_n_channels) result(image)

        implicit none

        integer,           intent(OUT) :: x, y, n_channels
        integer, optional, intent(IN)  :: req_n_channels
        character(*),      intent(IN)  :: filename
        integer, allocatable           :: image(:, :, :)

        character(kind=c_char,len=:), allocatable     :: t_filename
        character(kind=c_char), dimension(:), pointer :: char_array_pointer => null()
        integer, allocatable :: data_tmp(:)
        type(c_ptr)                               :: data_ptr
        integer                                   :: req_chan, i, err

        !assign filename in proper fashion for c interop
        t_filename = filename//c_null_char

        !currently don't support proper errors so check using stbi_info which does not decode image
        ! if err is 0 then problem reading image
        ! not true any more, can use stbi_failure_reason()
        err = stbi_info(t_filename, x, y, n_channels)
        if(err /= 1)then
            print*,"Image can not be read!"
            return
        end if

        if(present(req_n_channels))then
            req_chan = req_n_channels
        else
            req_chan = 0
        end if

        !read data
        data_ptr = c_stbi_load(t_filename, x, y, n_channels, req_chan)
        !check for errors
        if(.not. c_associated(data_ptr))then
            print*,stbi_failure_reason()
            return
        end if

        call c_f_pointer(data_ptr, char_array_pointer,[x*y*n_channels])
        allocate(data_tmp(x*y*n_channels))
        do i = 1, x*y*n_channels
            data_tmp(i) = ichar(char_array_pointer(i))
        end do

        !rehshape image data
        allocate(image(x,y,n_channels))
        image = reshape(data_tmp, shape=[x, y, n_channels], order=[3,1,2])

    end function stbi_load

    function stbi_loadf(filename, x, y, n_channels, req_n_channels) result(image)

        implicit none

        integer,           intent(OUT) :: x, y, n_channels
        integer, optional, intent(IN)  :: req_n_channels
        character(*),      intent(IN)  :: filename
        real,    allocatable           :: image(:, :, :)

        character(kind=c_char,len=1), allocatable :: t_filename(:)
        real(kind=c_float), pointer               :: float_array_pointer(:) => null()
        type(c_ptr)                               :: data_ptr
        integer                                   :: req_chan, i, err

        !assign filename in proper fashion for c interop
        allocate(t_filename(len(filename)))
        do i = 1, len(filename)
            t_filename(i) = filename(i:i)
        end do

        !currently don't support proper errors so check using stbi_info which does not decode image
        ! if err is 0 then problem reading image
        err = stbi_info(t_filename, x, y, n_channels)
        if(err /= 1)then
            print*,"Image can not be read!"
            return
        end if

        if(present(req_n_channels))then
            req_chan = req_n_channels
        else
            req_chan = 0
        end if

        !read data
        data_ptr = c_stbi_loadf(t_filename, x, y, n_channels, req_chan)
        !transfer c_ptr to f_ptr
        call c_f_pointer(data_ptr, float_array_pointer, [x*y*n_channels])

        !rehshape image data
        allocate(image(x,y,n_channels))
        image = reshape(float_array_pointer, shape=[x, y, n_channels], order=[3,1,2])

    end function stbi_loadf
end module stb_image_mod