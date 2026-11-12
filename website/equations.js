
const equations = {
  "bias": String.raw`
    \operatorname{Bias}(\hat{\theta}(t))
    = E[\hat{\theta}(t)] - \theta(t)
  `,

  "relative-bias": String.raw`
    \widehat{\operatorname{Relative\ Bias}}(t)
    =
    \frac{
      \frac{1}{N}\sum_{n=1}^{N}\hat{\theta}_n(t)
      - \theta(t)
    }{
      |\theta(t)|
    }
  `,

  "rmse": String.raw`
    \operatorname{RMSE}(t;\theta^*)
    =
    \sqrt{
      \frac{1}{N}
      \sum_{n=1}^{N}
      \left(
        \hat{\theta}_n(t)-\theta^*(t)
      \right)^2
    }
  `,

  "relative-rmse": String.raw`
    \operatorname{Relative\ RMSE}(t;\theta^*)
    =
    \sqrt{
      \frac{1}{N}
      \sum_{n=1}^{N}
      \left(
        \frac{
          \hat{\theta}_n(t)-\theta^*(t)
        }{
          |\theta^*(t)|
        }
      \right)^2
    }
  `
};

document.addEventListener("DOMContentLoaded", () => {
  document.querySelectorAll("[data-equation]").forEach(el => {
    const key = el.dataset.equation;
    const latex = equations[key];

    if (!latex) return;

    el.classList.add("math-tooltip");
    el.setAttribute("tabindex", "0");

    const popup = document.createElement("span");
    popup.className = "math-tooltip-content";
    popup.textContent = `\\(\\displaystyle ${latex}\\)`;

    el.appendChild(popup);
  });

  // Render the newly inserted equations with MathJax.
  if (window.MathJax?.typesetPromise) {
    window.MathJax.typesetPromise(
      Array.from(document.querySelectorAll(".math-tooltip-content"))
    );
  }
});
