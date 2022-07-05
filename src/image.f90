module image_mod

    implicit none

    type :: RGB
        integer :: R, G, B
    end type RGB

    type :: RGBA
        integer :: R, G, B, A
    end type RGBA

    type :: RGB_image
        type(RGB), dimension(:,:), pointer :: image
        integer :: width, height
        contains
        procedure :: save => save_fn_RGB
        procedure :: load => load_fn_RGB
    end type RGB_image
    
    interface RGB_image
        module procedure RGB_image_init
        module procedure RGB_image_alloc
    end interface RGB_image

    type :: RGBA_image
        type(RGBA), dimension(:,:), pointer :: image
        integer :: width, height
        contains
        procedure :: save => save_fn_RGBA
        procedure :: load => load_fn_RGBA
    end type RGBA_image

    interface RGBA_image
        module procedure RGBA_image_init
        module procedure RGBA_image_alloc
    end interface RGBA_image

    contains

        function RGB_image_init() result(img)

            type(RGB_image) :: img

            nullify(img%image)
            img%width = 0
            img%height = 0

        end function RGB_image_init

        function RGBA_image_init() result(img)

            type(RGBA_image) :: img

            nullify(img%image)
            img%width = 0
            img%height = 0

        end function RGBA_image_init

        function RGB_image_alloc(w, h) result(img)

            type(RGB_image) :: img
            integer, intent(IN) :: w, h

            allocate(img%image(w, h))
            img%width = w
            img%height = h

        end function RGB_image_alloc

        function RGBA_image_alloc(w, h) result(img)

            type(RGBA_image) :: img
            integer, intent(IN) :: w, h

            allocate(img%image(w, h))
            img%width = w
            img%height = h

        end function RGBA_image_alloc

        integer function save_fn_RGB(this, filename, quality)

            use stb_image_write_mod, only : write_image

            class(RGB_image),  intent(IN) :: this
            character(*),      intent(IN) :: filename
            integer, optional, intent(IN) :: quality
    
            integer :: data(this%width, this%height, 3)

            data(:, :, 1) = this%image(:, :)%R
            data(:, :, 2) = this%image(:, :)%G
            data(:, :, 3) = this%image(:, :)%B

            save_fn_RGB = write_image(filename, data, quality)
            
        end function save_fn_RGB

        integer function save_fn_RGBA(this, filename, quality)

            use stb_image_write_mod, only : write_image

            class(RGBA_image), intent(IN) :: this
            character(*),      intent(IN) :: filename
            integer, optional, intent(IN) :: quality
    
            integer :: data(this%width, this%height, 4)

            data(:, :, 1) = this%image(:, :)%R
            data(:, :, 2) = this%image(:, :)%G
            data(:, :, 3) = this%image(:, :)%B
            data(:, :, 4) = this%image(:, :)%A

            save_fn_RGBA = write_image(filename, data, quality)
            
        end function save_fn_RGBA


        function load_fn_RGB(this, filename) result(img)
            
            use, intrinsic :: iso_c_binding
            use stb_image_mod, only : stbi_info, stbi_load

            class(RGB_image), intent(IN) :: this
            character(*), intent(IN) :: filename
            type(RGB_image) :: img

            character(kind=c_char,len=:), allocatable :: t_filename
            integer, allocatable :: image(:, :, :)
            integer :: x, y, n_channels, err

            t_filename = filename//C_NULL_CHAR

            err = stbi_info(t_filename, x, y, n_channels)
            image = stbi_load(t_filename, x, y, n_channels, 0)

            img = RGB_image_alloc(x, y)

            img%image(:, :)%R = image(: , :, 1)
            img%image(:, :)%G = image(: , :, 2)
            img%image(:, :)%B = image(: , :, 3)

        end function load_fn_RGB

        function load_fn_RGBA(this, filename) result(img)
            
            use, intrinsic :: iso_c_binding
            use stb_image_mod, only : stbi_info, stbi_load

            class(RGBA_image), intent(IN) :: this
            character(*), intent(IN) :: filename
            type(RGBA_image) :: img

            character(kind=c_char,len=:), allocatable :: t_filename
            integer, allocatable :: image(:, :, :)
            integer :: x, y, n_channels, err

            t_filename = filename//C_NULL_CHAR

            err = stbi_info(t_filename, x, y, n_channels)
            image = stbi_load(t_filename, x, y, n_channels, 0)

            img = RGBA_image_alloc(x, y)

            img%image(:, :)%R = image(: , :, 1)
            img%image(:, :)%G = image(: , :, 2)
            img%image(:, :)%B = image(: , :, 3)
            if(size(image, dim=3) == 4)then
                img%image(:, :)%A = image(: , :, 4)
            else
                img%image(:, :)%A = 0
            end if
        end function load_fn_RGBA

end module image_mod