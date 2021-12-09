# Image data, 10 subjects with images as 100x100x100 pixels, 1 channel (monochromatic)
# Data are between 0,1 (corresponding to 0 to 255 rescaled)
library(keras)
library(tensorflow)
train_x <- runif(10*100*100*100*1, 0, 1)
train_x <- array_reshape(train_x, dim = c(10, 100, 100, 100, 1))

# Outcome data, binary
train_y <- to_categorical(sample(c(0,1), size = 10, replace = TRUE))
model <- keras_model_sequential()

model %>% 
  layer_conv_3d(filters = 10,
                kernel_size = c(3,3,3),
                input_shape = c(100, 100, 100, 1),
                data_format = 'channels_last') %>% 
  layer_flatten() %>% 
  layer_dense(units = 2, activation = 'softmax')

model %>% 
  compile(loss = 'binary_crossentropy',
          optimizer = 'adam',
          metrics = 'accuracy')

# Fit model
history <- 
  model %>%
  fit(train_x,
      train_y,
      epoch = 200,
      batch_size = 32,
      validation_split = 0.2)
