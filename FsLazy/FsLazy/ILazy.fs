namespace FsLazy

module ILazy =

    /// Lazy интерфейс
    type ILazy<'a> =
        /// Получение результата
        abstract member Get: unit -> 'a