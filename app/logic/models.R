box::use(
  keras3[keras_input, keras_model, layer_dense, layer_lstm, layer_reshape],
)

#' @export
create_model <- function(structure, inputvec, outputvec) {
  n_layers <- dim(structure)[2]
  input <- keras_input(shape = c(dim(inputvec)[2], dim(inputvec)[3]))

  x <- input
  for (layer in seq_len(n_layers)) {
    x <- x |>
      layer_lstm(
        units = as.numeric(structure[[layer]]),
        return_sequences = layer < n_layers
      )
  }

  output <- x |>
    layer_dense(dim(outputvec)[2] * dim(outputvec)[3]) |>
    layer_reshape(c(dim(outputvec)[2], dim(outputvec)[3]))

  keras_model(inputs = input, outputs = output)
}
