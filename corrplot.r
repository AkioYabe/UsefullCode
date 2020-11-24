cor(iris[,1:4],method = "spearman") %>%
  corrplot::corrplot(type = "upper"
                     ,method = "ellipse"
                     ,order = "hclust"#並び替え
                     ,addCoef.col = "gray10"#相関係数を文字で出力する際の色
                     ,tl.srt = 45#上段のtextの角度
                     )
