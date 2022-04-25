# Fortran_stb_image
Fortran bindings for [stb_image and stb_image_write](https://github.com/nothings/stb).

## Usage

    program test
    
    implicit none
    
    use stb_image_mod
    use stb_image
    
    integer :: data(100,100,3), x, y, n_channels, err
    
    data = 255
    data(:, :50, 1)=0
    ! write to png
    err = write_image("image.png", data)
    ! write to jpg with 80 qulaity
    err = write_image("image.jpg", data, 80)

    ! inquire image data without decoding image
    err = stbi_info("image.png", x, y, n_channels)

    ! load png image
    x = 100
    y = 100
    n_channels = 3
    image = stbi_load("image.tga", x, y, n_channels, 0)
    ! check loading failure reason 
    print*,stbi_failure_reason()
    
    end program
    
  
  See app/main.f90 for further examples.

## Building

  Currently only supports building via [fpm](https://github.com/fortran-lang/fpm)
  
## Status

  Currently not all of stb_image and stb_image_write implmented. The following functions are implemented:
  
  - stbi_write_bmp
  - stbi_write_hdr
  - stbi_write_png
  - stbi_write_tga
  - stbi_flip_vertically_on_write
  - stbi_set_flip_vertically_on_load
  - stbi_failure_reason
  - stbi_is_hdr
  - stbi_hdr_to_ldr_gamma
  - stbi_hdr_to_ldr_scale
  - stbi_ldr_to_hdr_gamma
  - stbi_ldr_to_hdr_scale
  - stbi_load
  - stbi_loadf
  - stbi_info
