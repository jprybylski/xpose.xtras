# Based on https://www.r-bloggers.com/2023/06/how-to-generate-a-hex-sticker-with-openai-and-cropcircles/
img_opts <- purrr::map(list.files(
  path = here::here("logo-creation"),
  pattern = "opt\\d\\.png",
  full.names = TRUE
), ~{
  img <- cropcircles::hex_crop(
    images = .x,
    border_colour = "#E5048C",
    border_size = 50
  )
  print(normalizePath(img,"/"))
  ggplot2::ggplot() +
    ggpath::geom_from_path(ggplot2::aes(0.5, 0.5, path = img)) +
    # package name
    ggplot2::annotate("text", x = 0.5, y = 0.78, label = "xpose.xtras", size = 6,
            fontface = "bold", colour = "#4B9ED7", angle = 0, hjust = 0.5, lineheight = 0.25) +
    # add github - remove if not wanted
    #annotate("richtext", x=0.46, y = 0.07, family = ft1, size = 10, angle = 15, colour = txt, hjust = 0,
    #         label = glue("<span style='font-family:fa-brands; color:{txt}'> </span> {git_name}"),
    #         label.color = NA, fill = NA) +
    ggplot2::xlim(0, 1) +
    ggplot2::ylim(0, 1) +
    ggplot2::theme_void()
})

figure_pick <- 2 # this one looks best, atm
if (!dir.exists(here::here("inst","figures"))) dir.create(here::here("inst","figures"))
hexSticker::save_sticker(here::here("inst","figures","logo.png"), img_opts[[figure_pick]])
rm(list=ls())
