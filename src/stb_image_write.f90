module stb_image_write_mod

    use, intrinsic :: iso_c_binding
    
    implicit none

    ! int stbi_write_tga_with_rle;             // defaults to true; set to 0 to disable RLE
    integer(c_int), bind(c, name="stbi_write_tga_with_rle") :: stbi_write_tga_with_rle
    ! int stbi_write_png_compression_level;    // defaults to 8; set to higher for more compression
    integer(c_int), bind(c, name="stbi_write_png_compression_level") :: stbi_write_png_compression_level
    ! int stbi_write_force_png_filter;         // defaults to -1; set to 0..5 to force a filter mode
    integer(c_int), bind(c, name="stbi_write_force_png_filter") :: stbi_write_force_png_filter

    !const void is a pointer, can convert const *int to const *void etc but not other way around
    interface
        ! int stbi_write_bmp(char const *filename, int w, int h, int comp, const void *data);
        function c_stbi_write_bmp(filename, w, h, comp, data) bind(c, name="stbi_write_bmp")
            import :: c_char, c_int, c_ptr
            character(kind=c_char), intent(IN) :: filename(*)
            integer(c_int), value,  intent(IN) :: w, h, comp
            type(c_ptr),    value              :: data
            integer(c_int) :: c_stbi_write_bmp
        end function c_stbi_write_bmp
    end interface
    
    interface
        ! int stbi_write_hdr(char const *filename, int w, int h, int comp, const float *data);
        function c_stbi_write_hdr(filename, w, h, comp, data) bind(c, name="stbi_write_hdr")
            import :: c_char, c_int, c_ptr
            character(kind=c_char), intent(IN) :: filename(*)
            integer(c_int), value,  intent(IN) :: w, h, comp
            type(c_ptr),    value              :: data
            integer(c_int) :: c_stbi_write_hdr
        end function c_stbi_write_hdr
    end interface

    interface
        ! int stbi_write_png(char const *filename, int w, int h, int comp, const void *data, int stride_in_bytes);
        function c_stbi_write_png(filename, w, h, comp, data, stride_in_bytes) bind(c, name="stbi_write_png")
            import :: c_char, c_int, c_ptr
            character(kind=c_char), intent(IN) :: filename(*)
            integer(c_int), value,  intent(IN) :: w, h, comp, stride_in_bytes
            type(c_ptr),    value              :: data
            integer(c_int) :: c_stbi_write_png
        end function c_stbi_write_png
    end interface

    interface
        ! int stbi_write_tga(char const *filename, int w, int h, int comp, const void *data);
        function c_stbi_write_tga(filename, w, h, comp, data) bind(c, name="stbi_write_tga")
            import :: c_char, c_int, c_ptr
            character(kind=c_char), intent(IN) :: filename(*)
            integer(c_int), value,  intent(IN) :: w, h, comp
            type(c_ptr),    value,  intent(IN) :: data
            integer(c_int) :: c_stbi_write_tga
        end function c_stbi_write_tga
    end interface

    interface
        ! int stbi_write_jpg(char const *filename, int w, int h, int comp, const void *data, int quality);
        function c_stbi_write_jpg(filename, w, h, comp, data, quality) bind(c, name="stbi_write_jpg")
            import :: c_char, c_int, c_ptr
            character(kind=c_char), intent(IN) :: filename(*)
            integer(c_int), value,  intent(IN) :: w, h, comp, quality
            type(c_ptr),    value              :: data
            integer(c_int) :: c_stbi_write_jpg
        end function c_stbi_write_jpg
    end interface

    interface
        ! void stbi_flip_vertically_on_write(int flag)
        subroutine c_stbi_flip_vertically_on_write(flag) bind(c, name="stbi_flip_vertically_on_write")
            import :: c_int
            integer(c_int), value, intent(IN) :: flag
        end subroutine c_stbi_flip_vertically_on_write
    end interface

    contains

        integer function write_image(filename, data, quality)
        
            use, intrinsic :: iso_c_binding

            implicit none

            character(*),      intent(IN) :: filename
            integer,           intent(IN) :: data(:, :, :)
            integer, optional, intent(IN) :: quality
            
            character(kind=c_char,len=:), allocatable         :: t_filename
            character(kind=c_char),       allocatable, target :: pixel(:)
            integer(c_int) :: x, y, comp, t_quality
            integer        :: size_img, i, j, k, idx, count
            type(c_ptr)    :: img

            if(present(quality))then
                t_quality = int(quality, kind=c_int)
            else
                t_quality = 100_c_int
            end if

            t_filename = filename//C_NULL_CHAR
            x = size(data, 1)
            y = size(data, 2)
            comp = size(data, 3)
            size_img = x * y * comp

            allocate(pixel(size_img))
            !convert w x h x c into w*h*c array in row major order for C
            count = 1
            do j = 1, y
                do i = 1, x
                    do k = 1, comp
                        pixel(count:count) = char(data(i,j,k))
                        count = count + 1
                    end do
                end do
            end do

            img = c_loc(pixel)

            idx = index(filename, ".png")            
            if(idx > 0)then
                write_image = c_stbi_write_png(t_filename, x, y, comp, img, 0_c_int)
                return
            end if

            idx = index(filename, ".tga")            
            if(idx > 0)then
                write_image = c_stbi_write_tga(t_filename, x, y, comp, img)
                return
            end if

            idx = index(filename, ".bmp")            
            if(idx > 0)then
                write_image = c_stbi_write_bmp(t_filename, x, y, comp, img)
                return
            end if

            idx = index(filename, ".jpg")            
            if(idx > 0)then
                write_image = c_stbi_write_jpg(t_filename, x, y, comp, img, 100_c_int)
                return
            end if

            idx = index(filename, ".hdr")            
            if(idx > 0)then
                write_image = c_stbi_write_hdr(t_filename, x, y, comp, img)
                return
            end if
            print*,"No such file type!"
            write_image = 0

        end function write_image
end module stb_image_write_mod
