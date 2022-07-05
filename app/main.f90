program stb_image
    
    use, intrinsic :: iso_c_binding

    use stb_image_mod
    use stb_image_write_mod

    implicit none
    
    integer(c_int) :: x, y, comp
    type(c_ptr) :: img
    character(kind=c_char), target :: pixel(200*100*3)
    character(kind=c_char,len=6) :: t_filename
    integer :: count,i,j,k

    real :: r,g,b
    integer :: ir, ig, ib
    integer :: index

    integer, allocatable :: data_tmp(:)
    integer :: data(10,10,3)


    data = 255
    data(:,:5,1)=0

    print*,"Written png",1==write_image("tmp.png", data)
    print*,"Written bmp",1==write_image("tmp.bmp", data)
    print*,"Written tga",1==write_image("tmp.tga", data)
    print*,"Written jpg",1==write_image("tmp.jpg", data)
    print*,"Written hdr",1==write_image("tmp.hdr", data)

    print*,"tmp.hdr is hdr",1==c_stbi_is_hdr("tmp.hdr")

    call c_stbi_hdr_to_ldr_gamma(2.2)
    call c_stbi_hdr_to_ldr_scale(1.0)

    img = c_stbi_load("data/200.png", x, y, comp, 3_c_int)
    
    t_filename = C_char_"1.tga"//C_NULL_CHAR
    index = c_stbi_write_tga(t_filename, x, y, comp, img)
    print*,"Written tga using ptr",1==index

    t_filename = C_char_"s.png"//C_NULL_CHAR
    index = c_stbi_write_png(t_filename, x, y, comp, img, 0_c_int)
    print*,"Written png using ptr",1==index

    t_filename = C_char_"1.jpg"//C_NULL_CHAR
    index = c_stbi_write_jpg(t_filename, x, y, comp, img, 100_c_int)
    print*,"Written jpg using ptr",1==index

    call c_stbi_flip_vertically_on_write(1_c_int)
    t_filename = C_char_"1.bmp"//C_NULL_CHAR
    index = c_stbi_write_bmp(t_filename, x, y, comp, img)
    print*,"Written bmp using ptr",1==index

    x = 200_c_int
    y = 100_c_int
    comp = 3_c_int
    index = 1

    pixel = char(255)
    do j = y,1,-1
        do i = 1,x
            r = real(i) / real(x)
            g = real(j) / real(y)
            b = 0.2
            ir = int(255.99*r)
            ig = int(255.99*g)
            ib = int(255.99*b)
            pixel(index) = char(ir)
            index = index + 1
            pixel(index) = char(ig)
            index = index + 1
            pixel(index) = char(ib)
            index = index + 1
        end do
    end do
    img = c_loc(pixel)
    t_filename = C_char_"2.png"//C_NULL_CHAR
    index = c_stbi_write_png(t_filename, x, y, comp, img, 0_c_int)
    print*,index

end program stb_image