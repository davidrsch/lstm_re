box::use(
  keras3[keras_model_sequential, layer_dense, layer_lstm, layer_reshape],
)

#' @export
create_model <- function(structure, inputvec, outputvec) {
  if (dim(structure)[2] == 1) {
    model <- keras_model_sequential() |>
      layer_lstm(
        units = as.numeric(structure[[1]]),
        input_shape = c(dim(inputvec)[2], dim(inputvec)[3])
      )
  } else {
    for (layer in seq_len(dim(structure)[2])) {
      if (layer == 1) {
        model <- keras_model_sequential() |>
          layer_lstm(
            units = as.numeric(structure[[1]]),
            input_shape = c(dim(inputvec)[2], dim(inputvec)[3]),
            return_sequences = TRUE
          )
      } else {
        if (layer > 1 && layer < dim(structure)[2]) {
          model <- model |>
            layer_lstm(
              units = as.numeric(structure[[layer]]),
              return_sequences = TRUE
            )
        } else {
          model <- model |>
            layer_lstm(units = as.numeric(structure[[layer]]))
        }
      }
    }
  }

  model <- model |>
    layer_dense(dim(outputvec)[2] * dim(outputvec)[3]) |>
    layer_reshape(c(dim(outputvec)[2], dim(outputvec)[3]))

  model
}
