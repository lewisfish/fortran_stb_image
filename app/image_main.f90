program test

    use image_mod

    implicit none
    
    type(RGB_image) :: imgRGB, img2RGB
    type(RGBA_image) :: imgRGBA, img2RGBA

    imgRGB = RGB_image()
    imgRGBA = RGBA_image()

    imgRGB = RGB_image(100, 100)
    imgRGBA = RGBA_image(101, 101)

    imgRGB%image = RGB(1, 1, 255)
    imgRGBA%image = RGBA(1, 1, 255, 1)

    imgRGBA%image(10:50, 10:50) = RGBA(1, 1, 255, 255)

    print*,imgRGB%save("test_new.png")
    print*,imgRGBA%save("test_new_RGBA.png")

    img2RGB = img2RGB%load("2.png")
    img2RGBA = img2RGBA%load("2.png")

    print*,img2RGB%image(10, 10),img2RGBA%image(10, 10)

end program test